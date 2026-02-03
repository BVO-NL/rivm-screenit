export const test = (expectation: string, assertion?: jasmine.ImplementationCallback, timeout?: number): void => {
  it(expectation, assertion, timeout)
}

export const suite = (description: string, specDefinitions: () => void): void => {
  describe(description, specDefinitions)
}
