type portToElm<T> = {
  send: (_: T) => void,
};
type portFromElm<T> = {
  subscribe: (_: (_: T) => void) => void
};

export const Elm: {
  Backend: {
    Main: {
      init: () => {
        ports: {
          requests: portToElm<{ request: Request, resolve: (_: Response | Promise<Response>) => void }>,
          typescriptHandler: portFromElm<{ request: Request, resolve: (_: Response | Promise<Response>) => void }>,
        }
      }
    }
  }
};
