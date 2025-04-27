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
      init: () => {
        ports: {
          requests: portToElm<{ request: Request, resolver: resolver }>,
          responses: portFromElm<{ resolver: resolver, status: number, body: string }>,
          taskRequests: portFromElm<any>,
          taskResponses: portToElm<any>,
          taskErrors: portFromElm<any>,
          typescriptHandoffs: portFromElm<{ request: Request, resolver: resolver, isAuthenticated: boolean }>,
        }
      }
    }
  }
};
