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
class ResizeObserver {
  observe() {}
  unobserve() {}
  disconnect() {}
}
globalThis.ResizeObserver = ResizeObserver as any

class IntersectionObserver {
  root: Element | Document | null
  rootMargin: string
  thresholds: number[]

  constructor(callback: IntersectionObserverCallback, options: IntersectionObserverInit = {}) {
    this.root = options.root ?? null
    this.rootMargin = options.rootMargin ?? '0px'
    this.thresholds = Array.isArray(options.threshold) ? options.threshold : [options.threshold ?? 0]
  }

  observe() {}
  unobserve() {}
  disconnect() {}
  takeRecords() {
    return []
  }
}
globalThis.IntersectionObserver = IntersectionObserver as any

class MockNotification extends EventTarget implements Notification {
  static permission: NotificationPermission = 'granted'
  static requestPermission(): Promise<NotificationPermission> {
    return Promise.resolve('granted')
  }
  static maxActions = 2

  badge = ''
  body = ''
  data: any = null
  dir: NotificationDirection = 'auto'
  icon = ''
  image = ''
  lang = ''
  renotify = false
  requireInteraction = false
  silent = false
  tag = ''
  timestamp = Date.now()
  title: string
  vibrate: readonly number[] = []
  onclick: ((this: Notification, ev: Event) => any) | null = null
  onclose: ((this: Notification, ev: Event) => any) | null = null
  onerror: ((this: Notification, ev: Event) => any) | null = null
  onshow: ((this: Notification, ev: Event) => any) | null = null

  constructor(title: string, options?: NotificationOptions) {
    super()
    this.title = title
    if (options) {
      Object.assign(this, options)
    }
  }

  close(): void {}
}
globalThis.Notification = MockNotification as any

class MockFileList implements FileList {
  [index: number]: File
  length: number

  constructor(files: File[] = []) {
    this.length = files.length
    files.forEach((file, i) => {
      this[i] = file
    })
  }

  item(index: number): File | null {
    return this[index] ?? null
  }

  [Symbol.iterator](): ArrayIterator<File> {
    let index = 0
    const length = this.length
    const self = this
    return {
      next(): IteratorResult<File> {
        if (index < length) {
          return { value: self[index++], done: false }
        }
        return { value: undefined as any, done: true }
      },
      [Symbol.iterator]() {
        return this
      },
      [Symbol.dispose]() {},
    } as ArrayIterator<File>
  }
}

class MockDataTransferItemList implements DataTransferItemList {
  [index: number]: DataTransferItem
  length = 0
  private _files: File[] = []

  constructor(private _dataTransfer: MockDataTransfer) {}

  add(data: string, type: string): DataTransferItem | null
  add(data: File): DataTransferItem | null
  add(data: any, type?: string): DataTransferItem | null {
    if (data instanceof File) {
      this._files.push(data)
      this.length = this._files.length
      this._dataTransfer._updateFiles(this._files)
    }
    return null
  }

  remove(index: number): void {
    if (index >= 0 && index < this._files.length) {
      this._files.splice(index, 1)
      this.length = this._files.length
      this._dataTransfer._updateFiles(this._files)
    }
  }

  clear(): void {
    this._files = []
    this.length = 0
    this._dataTransfer._updateFiles(this._files)
  }

  [Symbol.iterator](): ArrayIterator<DataTransferItem> {
    let index = 0
    const length = this.length
    const self = this
    return {
      next(): IteratorResult<DataTransferItem> {
        if (index < length) {
          return { value: self[index++], done: false }
        }
        return { value: undefined as any, done: true }
      },
      [Symbol.iterator]() {
        return this
      },
      [Symbol.dispose]() {},
    } as ArrayIterator<DataTransferItem>
  }
}

class MockDataTransfer implements DataTransfer {
  dropEffect: 'none' | 'copy' | 'link' | 'move' = 'none'
  effectAllowed: 'none' | 'copy' | 'copyLink' | 'copyMove' | 'link' | 'linkMove' | 'move' | 'all' | 'uninitialized' = 'all'
  files: FileList = new MockFileList()
  items: DataTransferItemList
  types: readonly string[] = []

  constructor() {
    this.items = new MockDataTransferItemList(this)
  }

  _updateFiles(files: File[]): void {
    this.files = new MockFileList(files)
  }

  clearData(format?: string): void {}
  getData(format: string): string {
    return ''
  }
  setData(format: string, data: string): void {}
  setDragImage(image: Element, x: number, y: number): void {}
}
globalThis.DataTransfer = MockDataTransfer as any
