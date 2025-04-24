import { HttpRequest, HttpResponse } from "./http";
import { DomError, DomElement, Viewport, SetViewportOptions, SetViewportOfOptions } from "./browser";
export * from "./http";
export * from "./browser";
export interface ElmPorts {
    send: {
        subscribe: (callback: (defs: TaskDefinition[] | Command) => Promise<void>) => void;
    };
    receive: {
        send: (result: TaskResult[] | PoolId) => void;
    };
}
export type Command = {
    command: "identify-pool";
};
export type PoolId = {
    poolId: number;
};
export type Tasks = {
    [fn: string]: (arg: any) => any;
};
export interface TaskDefinition {
    function: string;
    attemptId: string;
    taskId: string;
    args: any;
}
export interface TaskResult {
    attemptId: string;
    taskId: string;
    result: Success | Error;
}
export interface Success {
    value: any;
}
export interface Error {
    error: {
        reason: string;
        message: string;
        raw?: any;
    };
}
export interface Builtins {
    debugLog?: (message: string) => void;
    http?: (request: HttpRequest) => Promise<HttpResponse>;
    timeNow?: () => number;
    timeZoneOffset?: () => number;
    timeZoneName?: () => string | number;
    randomSeed?: () => number;
    sleep?: (ms: number) => Promise<void>;
    domFocus?: (id: string) => void | DomError;
    domBlur?: (id: string) => void | DomError;
    domGetViewport?: () => Viewport;
    domGetViewportOf?: (id: string) => Viewport | DomError;
    domSetViewport?: (args: SetViewportOptions) => void;
    domSetViewportOf?: (args: SetViewportOfOptions) => void | DomError;
    domGetElement?: (id: string) => DomElement | DomError;
}
export interface DebugOptions {
    taskStart?: boolean;
    taskFinish?: boolean;
}
export interface Options {
    tasks: Tasks;
    ports: ElmPorts;
    builtins?: Builtins;
    debug?: boolean | DebugOptions;
}
export declare function register(options: Options): void;
