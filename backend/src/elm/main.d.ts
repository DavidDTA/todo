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
          requests: portToElm<{ request: Request, resolve: resolver }>,
          responses: portFromElm<{ resolve: resolver, status: number, body: string }>,
          taskSend: portFromElm<any>,
          taskReceive: portToElm<any>,
          taskErrors: portFromElm<any>,
          typescriptHandler: portFromElm<{ request: Request, resolve: resolver }>,
        }
      }
    }
  }
};
