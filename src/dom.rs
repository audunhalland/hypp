trait Dom {
    type Element;
}

struct WebSys;

impl Dom for WebSys {
    type Element = web_sys::Element;
}
