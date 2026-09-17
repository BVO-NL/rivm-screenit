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
import { faUserCircle } from '@fortawesome/pro-light-svg-icons'
import { Bevolkingsonderzoek } from '@shared/types/bevolkingsonderzoek'

interface BvoNavigationItem {
  bvo: Bevolkingsonderzoek
  item: DsNavigationSidebarItem
}

const bvoNavigationItems: BvoNavigationItem[] = [
  {
    bvo: Bevolkingsonderzoek.CERVIX,
    item: ({
      label: 'Baarmoederhalskanker',
      icon: 'femalereproductive-system-outline',
      routerLink: '/client/dossier/baarmoederhalskanker',
    } as unknown) as DsNavigationSidebarItem,
  },
  {
    bvo: Bevolkingsonderzoek.MAMMA,
    item: ({
      label: 'Borstkanker',
      icon: 'breasts-outline',
      routerLink: '/client/dossier/borstkanker',
    } as unknown) as DsNavigationSidebarItem,
  },
  {
    bvo: Bevolkingsonderzoek.COLON,
    item: ({
      label: 'Darmkanker',
      icon: 'intestine-outline',
      routerLink: '/client/dossier/darmkanker',
    } as unknown) as DsNavigationSidebarItem,
  },
]

export const getClientDossierNavigationItems = (actieveBvos: Bevolkingsonderzoek[]): (DsNavigationSidebarItem | DsNavigationSidebarCategoryItem)[] => {
  const items: (DsNavigationSidebarItem | DsNavigationSidebarCategoryItem)[] = [
    ({
      label: 'Algemeen',
      menuItems: [
        ({
          label: 'Overzicht',
          icon: faUserCircle,
          routerLink: '/client/dossier/overzicht',
        } as DsNavigationSidebarItem),
      ] as (DsNavigationSidebarItem | DsNavigationSidebarCategoryItem)[],
    } as DsNavigationSidebarCategoryItem),
  ]

  const bvoMenuItems = bvoNavigationItems.filter(({ bvo }) => actieveBvos.includes(bvo)).map(({ item }) => item)

  if (bvoMenuItems.length > 0) {
    items.push(({
      label: 'Bevolkingsonderzoek',
      menuItems: bvoMenuItems,
    } as DsNavigationSidebarCategoryItem))
  }

  return items
}
