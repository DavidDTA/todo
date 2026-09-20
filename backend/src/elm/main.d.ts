type portToElm<T> = {
  send: (_: T) => void,
};
type portFromElm<T> = {
  subscribe: (_: (_: T) => void) => void
};
type resolver = (_: Response | Promise<Response>) => void

export const Elm: {
  Backend: {
    Main: {
      init: (_: { flags: { globalThis: any } }) => {
        ports: {
          requests: portToElm<{ request: Request, resolver: resolver }>,
          taskRequests: portFromElm<any>,
          taskResponses: portToElm<any>,
          errors: portFromElm<any[]>,
        }
      }
    }
  }
};
