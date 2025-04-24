export function focus(id) {
    return withDomNode(id, (el) => el.focus());
}
export function blur(id) {
    return withDomNode(id, (el) => el.blur());
}
export function getViewport() {
    return {
        scene: getBrowserScene(),
        viewport: {
            x: window.scrollX,
            y: window.scrollY,
            width: document.documentElement.clientWidth,
            height: document.documentElement.clientHeight,
        },
    };
}
export function getViewportOf(id) {
    return withDomNode(id, (el) => ({
        scene: {
            width: el.scrollWidth,
            height: el.scrollHeight,
        },
        viewport: {
            x: el.scrollLeft,
            y: el.scrollTop,
            width: el.clientWidth,
            height: el.clientHeight,
        },
    }));
}
export function setViewport(options) {
    window.scroll(options.y, options.y);
}
export function setViewportOf(options) {
    return withDomNode(options.id, (el) => {
        el.scrollLeft = options.x;
        el.scrollTop = options.y;
    });
}
export function getElement(id) {
    return withDomNode(id, (el) => {
        const rect = el.getBoundingClientRect();
        const x = window.scrollX;
        const y = window.scrollY;
        return {
            scene: getBrowserScene(),
            viewport: {
                x: x,
                y: y,
                width: document.documentElement.clientWidth,
                height: document.documentElement.clientHeight,
            },
            element: {
                x: x + rect.left,
                y: y + rect.top,
                width: rect.width,
                height: rect.height,
            },
        };
    });
}
function withDomNode(id, callback) {
    const el = document.getElementById(id);
    if (el) {
        return callback(el);
    }
    return { error: null };
}
function getBrowserScene() {
    const body = document.body;
    const elem = document.documentElement;
    return {
        width: Math.max(body.scrollWidth, body.offsetWidth, elem.scrollWidth, elem.offsetWidth, elem.clientWidth),
        height: Math.max(body.scrollHeight, body.offsetHeight, elem.scrollHeight, elem.offsetHeight, elem.clientHeight),
    };
}
//# sourceMappingURL=dom.js.map