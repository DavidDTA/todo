import { Viewport, DomElement, DomError, SetViewportOptions, SetViewportOfOptions } from "browser";
export declare function focus(id: string): void | DomError;
export declare function blur(id: string): void | DomError;
export declare function getViewport(): Viewport;
export declare function getViewportOf(id: string): Viewport | DomError;
export declare function setViewport(options: SetViewportOptions): void;
export declare function setViewportOf(options: SetViewportOfOptions): void | DomError;
export declare function getElement(id: string): DomElement | DomError;
