package nl.rivm.screenit.batch.service.impl.dicom;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-bk
 * %%
 * Copyright (C) 2012 - 2026 Facilitaire Samenwerking Bevolkingsonderzoek
 * %%
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 * =========================LICENSE_END==================================
 */

import java.io.File;
import java.io.IOException;
import java.io.StringReader;
import java.security.GeneralSecurityException;
import java.util.HashMap;
import java.util.Properties;
import java.util.concurrent.Executors;

import nl.rivm.screenit.model.mamma.dicom.CStoreConfig;
import nl.rivm.screenit.model.mamma.dicom.SCPConfig;

import org.dcm4che3.data.Attributes;
import org.dcm4che3.data.ElementDictionary;
import org.dcm4che3.data.Tag;
import org.dcm4che3.data.UID;
import org.dcm4che3.io.DicomInputStream;
import org.dcm4che3.net.ApplicationEntity;
import org.dcm4che3.net.Association;
import org.dcm4che3.net.Connection;
import org.dcm4che3.net.DataWriterAdapter;
import org.dcm4che3.net.Device;
import org.dcm4che3.net.DimseRSPHandler;
import org.dcm4che3.net.IncompatibleConnectionException;
import org.dcm4che3.net.Priority;
import org.dcm4che3.net.Status;
import org.dcm4che3.net.pdu.AAssociateRQ;
import org.dcm4che3.net.pdu.CommonExtendedNegotiation;
import org.dcm4che3.net.pdu.PresentationContext;
import org.dcm4che3.util.SafeClose;
import org.dcm4che3.util.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class CStoreSCU extends Device
{
	private static final Logger LOG = LoggerFactory.getLogger(CStoreSCU.class);

	private RelatedGeneralSOPClasses relatedSOPClasses = new RelatedGeneralSOPClasses();

	private CStoreConfig dicomCStoreConfig;

	private Attributes fileAttributes;

	private Attributes response;

	public CStoreSCU(CStoreConfig dicomCStoreConfig)
	{
		super(dicomCStoreConfig.getScuConfig().getAeTitle());
		this.dicomCStoreConfig = dicomCStoreConfig;
	}

	public boolean store(File file, String sopClasses, Long accessionNumber, String bsn)
	{
		var ae = new ApplicationEntity(dicomCStoreConfig.getScuConfig().getAeTitle());
		var conn = new Connection();
		var remote = new Connection();
		var rq = new AAssociateRQ();
		Association as = null;

		addConnection(conn);
		addApplicationEntity(ae);
		ae.addConnection(conn);

		configureRemoteConnection(remote, rq, dicomCStoreConfig.getScpConfig());
		configureLocalConnection(conn);

		try
		{
			var p = new Properties();
			p.load(new StringReader(sopClasses));
			relatedSOPClasses.init(p);

			try (var dicomInputStream = new DicomInputStream(file))
			{
				dicomInputStream.setIncludeBulkData(DicomInputStream.IncludeBulkData.NO);
				var attributes = dicomInputStream.readFileMetaInformation();
				var ds = dicomInputStream.readDatasetUntilPixelData();
				if (attributes == null || !attributes.containsValue(Tag.TransferSyntaxUID)
					|| !attributes.containsValue(Tag.MediaStorageSOPClassUID)
					|| !attributes.containsValue(Tag.MediaStorageSOPInstanceUID))
				{
					attributes = ds.createFileMetaInformation(dicomInputStream.getTransferSyntax());
				}

				var cuid = attributes.getString(Tag.MediaStorageSOPClassUID);
				var iuid = attributes.getString(Tag.MediaStorageSOPInstanceUID);
				var ts = attributes.getString(Tag.TransferSyntaxUID);

				if (!rq.containsPresentationContextFor(cuid))
				{
					rq.addCommonExtendedNegotiation(relatedSOPClasses
						.getCommonExtendedNegotiation(cuid));

					if (!ts.equals(UID.ExplicitVRLittleEndian))
					{
						rq.addPresentationContext(new PresentationContext(rq
							.getNumberOfPresentationContexts() * 2 + 1, cuid,
							UID.ExplicitVRLittleEndian));
					}
					if (!ts.equals(UID.ImplicitVRLittleEndian))
					{
						rq.addPresentationContext(new PresentationContext(rq
							.getNumberOfPresentationContexts() * 2 + 1, cuid,
							UID.ImplicitVRLittleEndian));
					}
				}
				rq.addPresentationContext(new PresentationContext(rq
					.getNumberOfPresentationContexts() * 2 + 1, cuid, ts));

				var executorService = Executors.newSingleThreadExecutor();
				var scheduledExecutorService = Executors.newSingleThreadScheduledExecutor();
				setExecutor(executorService);
				setScheduledExecutor(scheduledExecutorService);

				try
				{
					as = open(conn, remote, ae, rq);
					sendFile(file, as, iuid, cuid, ts, accessionNumber, bsn);
					return true;
				}
				catch (InterruptedException | IncompatibleConnectionException | GeneralSecurityException | IOException e)
				{
					LOG.error("Fout bij uploaden beelden", e);
					if (e instanceof InterruptedException)
					{
						Thread.currentThread().interrupt();
					}
				}
				finally
				{
					try
					{
						close(as);
					}
					catch (InterruptedException | IOException e)
					{
						LOG.error("Fout bij uploaden beelden", e);
						if (e instanceof InterruptedException)
						{
							Thread.currentThread().interrupt();
						}
					}
					executorService.shutdown();
					scheduledExecutorService.shutdown();
				}
			}
		}
		catch (IOException e)
		{
			LOG.error("Fout bij uploaden beelden", e);
		}

		return false;
	}

	private void configureRemoteConnection(Connection conn, AAssociateRQ rq, SCPConfig scpConfig)
	{
		conn.setPort(scpConfig.getPoort());
		rq.setCalledAET(scpConfig.getAeTitle());
		conn.setHostname(scpConfig.getHost());
	}

	private void configureLocalConnection(Connection conn)
	{
		conn.setReceivePDULength(Connection.DEF_MAX_PDU_LENGTH);
		conn.setSendPDULength(Connection.DEF_MAX_PDU_LENGTH);
		conn.setMaxOpsInvoked(0);
		conn.setMaxOpsPerformed(0);
		conn.setPackPDV(true);
		conn.setConnectTimeout(1000); 
		conn.setRequestTimeout(1000); 
		conn.setAcceptTimeout(2000); 
		conn.setReleaseTimeout(1000); 
		conn.setResponseTimeout(1000 * 60 * 10); 
		conn.setIdleTimeout(0);
		conn.setSocketCloseDelay(Connection.DEF_SOCKETDELAY);
		conn.setSendBufferSize(0);
		conn.setReceiveBufferSize(0);
		conn.setTcpNoDelay(true);
	}

	private Association open(Connection conn, Connection remote, ApplicationEntity ae, AAssociateRQ rq)
		throws IOException, InterruptedException, IncompatibleConnectionException, GeneralSecurityException
	{
		return ae.connect(conn, remote, rq);
	}

	private void sendFile(File file, Association as, String iuid, String cuid, String ts, Long accessionNumber, String bsn) throws IOException, InterruptedException
	{
		send(file, cuid, iuid, ts, as, accessionNumber, bsn);
		as.waitForOutstandingRSP();
	}

	private void send(final File f, String cuid, String iuid,
		String filets, Association as, Long accessionNumber, String bsn) throws IOException, InterruptedException
	{
		var ts = selectTransferSyntax(cuid, filets, as);

		var in = new DicomInputStream(f);
		try
		{
			in.setIncludeBulkData(DicomInputStream.IncludeBulkData.URI);
			var data = in.readDataset();

			data.setString(Tag.AccessionNumber, ElementDictionary.vrOf(Tag.AccessionNumber, data.getPrivateCreator(Tag.AccessionNumber)), String.valueOf(accessionNumber));
			data.setString(Tag.StudyID, ElementDictionary.vrOf(Tag.StudyID, data.getPrivateCreator(Tag.StudyID)), String.valueOf(accessionNumber));
			data.setString(Tag.CodeValue, ElementDictionary.vrOf(Tag.CodeValue, data.getPrivateCreator(Tag.CodeValue)), "ZHOND");
			data.setString(Tag.OperatorsName, ElementDictionary.vrOf(Tag.OperatorsName, data.getPrivateCreator(Tag.OperatorsName)));

			var oldPatientId = data.getString(Tag.PatientID, null);
			var oldIssuerOfPatientId = data.getString(Tag.IssuerOfPatientID, null);
			data.setString(Tag.PatientID, ElementDictionary.vrOf(Tag.PatientID, data.getPrivateCreator(Tag.PatientID)), bsn);
			data.setString(Tag.IssuerOfPatientID, ElementDictionary.vrOf(Tag.IssuerOfPatientID, data.getPrivateCreator(Tag.IssuerOfPatientID)), "2.16.840.1.113883.2.4.6.3");

			var sq = new Attributes();
			sq.setString(Tag.PatientID, ElementDictionary.vrOf(Tag.PatientID, data.getPrivateCreator(Tag.PatientID)), oldPatientId);
			var sq2 = new Attributes();
			sq2.setString(Tag.IssuerOfPatientID, ElementDictionary.vrOf(Tag.IssuerOfPatientID, data.getPrivateCreator(Tag.IssuerOfPatientID)), oldIssuerOfPatientId);

			var sequence = data.newSequence(Tag.OtherPatientIDsSequence, 2);
			sequence.add(sq);
			sequence.add(sq2);

			setFileAttributes(data);

			as.cstore(cuid, iuid, Priority.NORMAL,
				new DataWriterAdapter(data), ts,
				new DimseRSPHandler(as.nextMessageID())
				{

					@Override
					public void onDimseRSP(Association as, Attributes cmd,
						Attributes data)
					{
						super.onDimseRSP(as, cmd, data);
						CStoreSCU.this.onCStoreRSP(cmd);
					}
				});
		}
		finally
		{
			SafeClose.close(in);
		}
	}

	private String selectTransferSyntax(String cuid, String filets, Association as)
	{
		var tss = as.getTransferSyntaxesFor(cuid);
		if (tss.contains(filets))
		{
			return filets;
		}

		if (tss.contains(UID.ExplicitVRLittleEndian))
		{
			return UID.ExplicitVRLittleEndian;
		}

		return UID.ImplicitVRLittleEndian;
	}

	private void onCStoreRSP(Attributes cmd)
	{
		this.response = cmd;

		var status = cmd.getInt(Tag.Status, -1);
		switch (status)
		{
		case Status.Success:
			LOG.info("Succes");
			break;
		case Status.CoercionOfDataElements:
		case Status.ElementsDiscarded:
		case Status.DataSetDoesNotMatchSOPClassWarning:
			LOG.warn("Warning: {}", cmd);
			break;
		default:
			LOG.error("Error: {}", cmd);
		}
	}

	private void close(Association as) throws IOException, InterruptedException
	{
		if (as != null && as.isReadyForDataTransfer())
		{
			as.waitForOutstandingRSP();
			as.release();
		}
	}

	public Attributes getResponse()
	{
		return response;
	}

	private void setFileAttributes(Attributes fileAttributes)
	{
		this.fileAttributes = fileAttributes;
	}

	public Attributes getFileAttributes()
	{
		return fileAttributes;
	}

	private static class RelatedGeneralSOPClasses
	{
		private final HashMap<String, CommonExtendedNegotiation> commonExtNegs = new HashMap<String, CommonExtendedNegotiation>();

		public void init(Properties props)
		{
			for (var cuid : props.stringPropertyNames())
			{
				commonExtNegs.put(cuid, new CommonExtendedNegotiation(cuid,
					UID.Storage,
					StringUtils.split(props.getProperty(cuid), ',')));
			}

		}

		private CommonExtendedNegotiation getCommonExtendedNegotiation(String cuid)
		{
			var commonExtNeg = commonExtNegs.get(cuid);
			return commonExtNeg != null
				? commonExtNeg
				: new CommonExtendedNegotiation(cuid, UID.Storage);
		}
	}
}
