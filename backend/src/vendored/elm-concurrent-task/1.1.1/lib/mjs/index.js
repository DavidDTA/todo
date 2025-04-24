import * as fetchAdapter from "./http/fetch";
import * as dom from "./browser/dom";
export * from "./http";
export * from "./browser";
const BuiltInTasks = {
    debugLog: console.log,
    http: fetchAdapter.http,
    timeNow: () => Date.now(),
    timeZoneOffset: () => getTimezoneOffset(),
    timeZoneName: () => getTimeZoneName(),
    randomSeed: () => Date.now(),
    sleep: sleep,
    domFocus: dom.focus,
    domBlur: dom.blur,
    domGetViewport: dom.getViewport,
    domGetViewportOf: dom.getViewportOf,
    domSetViewport: dom.setViewport,
    domSetViewportOf: dom.setViewportOf,
    domGetElement: dom.getElement,
};
function sleep(ms) {
    return new Promise((resolve) => setTimeout(resolve, ms));
}
function getTimezoneOffset() {
    return -new Date().getTimezoneOffset();
}
function getTimeZoneName() {
    try {
        return Intl.DateTimeFormat().resolvedOptions().timeZone;
    }
    catch (e) {
        return new Date().getTimezoneOffset();
    }
}
export function register(options) {
    const tasks = createTasks(options);
    const subscribe = options.ports.send.subscribe;
    const send = options.ports.receive.send;
    let poolId = 0;
    function nextPoolId() {
        poolId = cycleInt({ max: 1000 }, poolId);
    }
    subscribe(async (payload) => {
        if ("command" in payload) {
            switch (payload.command) {
                case "identify-pool": {
                    return Promise.resolve().then(() => {
                        send({ poolId });
                        nextPoolId();
                    });
                }
                default: {
                    throw new Error(`Unrecognised internal command: ${payload}`);
                }
            }
        }
        else {
            const debouncedSend = debounce(send, debounceThreshold(payload));
            for (const def of payload) {
                if (!tasks[def.function]) {
                    return debouncedSend({
                        attemptId: def.attemptId,
                        taskId: def.taskId,
                        result: {
                            error: {
                                reason: "missing_function",
                                message: `${def.function} is not registered`,
                            },
                        },
                    });
                }
            }
            payload.map(async (def) => {
                try {
                    logTaskStart(def, options);
                    const result = await tasks[def.function]?.(def.args);
                    logTaskFinish(def, options);
                    debouncedSend({
                        attemptId: def.attemptId,
                        taskId: def.taskId,
                        result: { value: result },
                    });
                }
                catch (e) {
                    debouncedSend({
                        attemptId: def.attemptId,
                        taskId: def.taskId,
                        result: {
                            error: {
                                reason: "js_exception",
                                message: `${e.name}: ${e.message}`,
                                raw: e,
                            },
                        },
                    });
                }
            });
        }
    });
}
function logTaskStart(def, options) {
    const logStart = options.debug &&
        typeof options.debug !== "boolean" &&
        options.debug.taskStart;
    if (logStart || options.debug === true) {
        console.info(`--starting-- ${def.function} attempt-${def.attemptId} id-${def.taskId}`);
    }
}
function logTaskFinish(def, options) {
    const logStart = options.debug &&
        typeof options.debug !== "boolean" &&
        options.debug.taskFinish;
    if (logStart || options.debug === true) {
        console.info(`--complete-- ${def.function} attempt - ${def.attemptId} id - ${def.taskId}`);
    }
}
function debounceThreshold(defs) {
    return defs.length > 10 ? 20 : 0;
}
function debounce(send, wait) {
    let timeout;
    let results = [];
    return function enqueueResult(taskResult) {
        results.push(taskResult);
        const later = () => {
            clearTimeout(timeout);
            send(results);
            results = [];
        };
        clearTimeout(timeout);
        timeout = setTimeout(later, wait);
    };
}
function createTasks(options) {
    const builtins = {
        ...BuiltInTasks,
        ...(options.builtins || {}),
    };
    return {
        ...prefixWith("builtin:", builtins),
        ...options.tasks,
    };
}
function prefixWith(prefix, tasks) {
    return Object.fromEntries(Object.entries(tasks).map(([key, fn]) => [`${prefix}${key}`, fn]));
}
function cycleInt(options, i) {
    return i >= options.max ? 0 : i + 1;
}
//# sourceMappingURL=index.js.map