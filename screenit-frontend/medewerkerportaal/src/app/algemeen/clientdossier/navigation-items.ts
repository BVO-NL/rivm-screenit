/*-
 * ========================LICENSE_START=================================
 * medewerkerportaal
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
import { DsNavigationSidebarCategoryItem, DsNavigationSidebarItem } from '@topicus-rgp-ds/web'
import { IconDefinition } from '@fortawesome/fontawesome-svg-core'
import { faBreadLoaf, faCandyCane, faStomach, faUserCircle } from '@fortawesome/pro-light-svg-icons'
import { Bevolkingsonderzoek } from '@shared/types/autorisatie/bevolkingsonderzoek'

interface BvoNavigationItem {
  bvo: Bevolkingsonderzoek
  item: DsNavigationSidebarItem
}

const bvoNavigationItems: BvoNavigationItem[] = [
  {
    bvo: Bevolkingsonderzoek.CERVIX,
    item: <DsNavigationSidebarItem>{
      label: 'Baarmoederhalskanker',
      icon: faCandyCane as IconDefinition,
      routerLink: '/client-dossier/baarmoederhalskanker',
    },
  },
  {
    bvo: Bevolkingsonderzoek.MAMMA,
    item: <DsNavigationSidebarItem>{
      label: 'Borstkanker',
      icon: faBreadLoaf as IconDefinition,
      routerLink: '/client/dossier/borstkanker',
    },
  },
  {
    bvo: Bevolkingsonderzoek.COLON,
    item: <DsNavigationSidebarItem>{
      label: 'Darmkanker',
      icon: faStomach as IconDefinition,
      routerLink: '/client/dossier/darmkanker',
    },
  },
]

export const getClientDossierNavigationItems = (actieveBvos: Bevolkingsonderzoek[]): (DsNavigationSidebarItem | DsNavigationSidebarCategoryItem)[] => {
  const items: (DsNavigationSidebarItem | DsNavigationSidebarCategoryItem)[] = [
    <DsNavigationSidebarCategoryItem>{
      label: 'Algemeen',
      menuItems: <(DsNavigationSidebarItem | DsNavigationSidebarCategoryItem)[]>[
        <DsNavigationSidebarItem>{
          label: 'Overzicht',
          icon: faUserCircle as IconDefinition,
          routerLink: '/client/dossier/overzicht',
        },
      ],
    },
  ]

  const bvoMenuItems = bvoNavigationItems.filter(({ bvo }) => actieveBvos.includes(bvo)).map(({ item }) => item)

  if (bvoMenuItems.length > 0) {
    items.push(<DsNavigationSidebarCategoryItem>{
      label: 'Bevolkingsonderzoek',
      menuItems: bvoMenuItems,
    })
  }

  return items
}
