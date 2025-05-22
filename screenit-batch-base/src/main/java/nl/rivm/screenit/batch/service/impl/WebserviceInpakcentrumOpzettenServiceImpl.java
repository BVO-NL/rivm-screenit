package nl.rivm.screenit.batch.service.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-base
 * %%
 * Copyright (C) 2012 - 2025 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.util.List;

import jakarta.xml.ws.BindingProvider;

import nl.rivm.screenit.batch.service.WebserviceInpakcentrumOpzettenService;
import nl.rivm.screenit.util.logging.cxf.ScreenITLoggingInInterceptor;
import nl.rivm.screenit.util.logging.cxf.ScreenITLoggingOutInterceptor;
import nl.rivm.screenit.util.logging.cxf.ScreenITLoggingSaver;

import org.apache.cxf.configuration.jsse.TLSClientParameters;
import org.apache.cxf.endpoint.ConduitSelector;
import org.apache.cxf.endpoint.PreexistingConduitSelector;
import org.apache.cxf.frontend.ClientProxy;
import org.apache.cxf.transport.http.HTTPConduit;
import org.apache.cxf.transports.http.configuration.HTTPClientPolicy;
import org.apache.cxf.ws.policy.PolicyConstants;
import org.apache.neethi.Policy;
import org.apache.neethi.PolicyComponent;
import org.springframework.beans.factory.annotation.Autowired;
import org.tempuri.DaklapackWebService;
import org.tempuri.IUpload;

public class WebserviceInpakcentrumOpzettenServiceImpl implements WebserviceInpakcentrumOpzettenService
{
	@Autowired
	private ScreenITLoggingSaver loggingSaver;

	@Autowired
	private String inpakCentrumEndpointUrl;

	@Autowired
	private Boolean testModus;

	@Override
	public IUpload initialiseerWebserviceInpakcentrum()
	{
		var webserviceClient = new DaklapackWebService();
		var upload = webserviceClient.getDataUploadEndpoint();
		var bindingProvider = (BindingProvider) upload;
		var requestContext = bindingProvider.getRequestContext();
		requestContext.put(BindingProvider.ENDPOINT_ADDRESS_PROPERTY, inpakCentrumEndpointUrl);

		if (Boolean.TRUE.equals(testModus) && inpakCentrumEndpointUrl.startsWith("http:"))
		{
			requestContext.put(PolicyConstants.POLICY_OVERRIDE, new HttpPolicy());
		}

		var client = ClientProxy.getClient(upload);
		client.getInInterceptors().add(new ScreenITLoggingInInterceptor(loggingSaver));
		client.getOutInterceptors().add(new ScreenITLoggingOutInterceptor(loggingSaver));

		var httpConduit = (HTTPConduit) client.getConduit();

		var tlsClientParameters = new TLSClientParameters();
		tlsClientParameters.setDisableCNCheck(true);

		httpConduit.setTlsClientParameters(tlsClientParameters);

		var policy = new HTTPClientPolicy();
		policy.setVersion("1.1");
		policy.setConnectionTimeout(150000);
		policy.setReceiveTimeout(150000);
		httpConduit.setClient(policy);

		ConduitSelector selector = new PreexistingConduitSelector(httpConduit, client.getEndpoint());
		client.setConduitSelector(selector);

		return upload;
	}

	private static class HttpPolicy extends Policy
	{
		@Override
		public void addPolicyComponent(PolicyComponent component)
		{

		}

		@Override
		public void addPolicyComponents(List<? extends PolicyComponent> components)
		{

		}
	}
}
