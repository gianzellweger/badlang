// .wrangler/tmp/bundle-WPCyc0/checked-fetch.js
var urls = /* @__PURE__ */ new Set();
function checkURL(request, init) {
  const url = request instanceof URL ? request : new URL(
    (typeof request === "string" ? new Request(request, init) : request).url
  );
  if (url.port && url.port !== "443" && url.protocol === "https:") {
    if (!urls.has(url.toString())) {
      urls.add(url.toString());
      console.warn(
        `WARNING: known issue with \`fetch()\` requests to custom HTTPS ports in published Workers:
 - ${url.toString()} - the custom port will be ignored when the Worker is published using the \`wrangler deploy\` command.
`
      );
    }
  }
}
globalThis.fetch = new Proxy(globalThis.fetch, {
  apply(target, thisArg, argArray) {
    const [request, init] = argArray;
    checkURL(request, init);
    return Reflect.apply(target, thisArg, argArray);
  }
});

// build/worker/shim.mjs
import $ from "./44fa2a6cffc343d8a55dcf810e6812b2f135c88a-index.wasm";
import oe from "./44fa2a6cffc343d8a55dcf810e6812b2f135c88a-index.wasm";
var C = Object.defineProperty;
var I = (t, e) => {
  for (var n in e)
    C(t, n, { get: e[n], enumerable: true });
};
var b = {};
I(b, { IntoUnderlyingByteSource: () => O, IntoUnderlyingSink: () => M, IntoUnderlyingSource: () => E, MinifyConfig: () => S, PolishConfig: () => G, R2Range: () => T, RequestRedirect: () => V, __wbg_buffer_12d079cc21e14bdb: () => vt, __wbg_buffer_dd7f74bc60f1faab: () => Jt, __wbg_byobRequest_72fca99f9c32c193: () => ct, __wbg_byteLength_58f7b4fab1919d44: () => Vt, __wbg_byteOffset_81d60f7392524f62: () => Gt, __wbg_call_27c0f87801dedf93: () => Ot, __wbg_call_b3ca7c6051f9bec1: () => $t, __wbg_cause_3d9c85ebaf6b1155: () => Ct, __wbg_cf_c62699e22b0d5280: () => st, __wbg_close_184931724d961ccc: () => ut, __wbg_close_a994f9425dab445c: () => at, __wbg_enqueue_ea194723156c0cc2: () => bt, __wbg_error_8e3928cfb8a43e2b: () => ht, __wbg_globalThis_d1e6af4856ba331b: () => Tt, __wbg_global_207b558942527489: () => qt, __wbg_headers_abb199c3be8d817c: () => wt, __wbg_instanceof_Error_e20bb56fd5591a93: () => At, __wbg_length_c20a40f15020d68a: () => Ht, __wbg_method_83327ed2e3f3229c: () => dt, __wbg_new_28c511d9baebfa89: () => Lt, __wbg_new_72fb9a18b5ae2624: () => Mt, __wbg_new_81740750da40724f: () => Dt, __wbg_new_ab6fd82b10560829: () => pt, __wbg_newnoargs_e258087cd0daa0ea: () => jt, __wbg_newwithbyteoffsetandlength_aa4a17c33a06e5cb: () => Nt, __wbg_newwithlength_e9b4878cebadb3d3: () => Bt, __wbg_newwithoptbuffersourceandinit_a4fa81e77259bb96: () => mt, __wbg_newwithoptreadablestreamandinit_0b825f969ca543d6: () => kt, __wbg_newwithoptstrandinit_219732174c595a25: () => xt, __wbg_queueMicrotask_3cbae2ec6b6cd3d6: () => Ft, __wbg_queueMicrotask_481971b0d87f3dd4: () => zt, __wbg_resolve_b0083a7967828ec8: () => Ut, __wbg_respond_b1a43b2e3a06d525: () => gt, __wbg_self_ce0dbfc45cf2f5be: () => Et, __wbg_set_1f9b04f170055d33: () => Kt, __wbg_set_a47bac70306a19a7: () => Pt, __wbg_set_cb0e7a5c2dd66afd: () => yt, __wbg_then_0c86a60e8fcfe9f6: () => Wt, __wbg_toString_ffe4c9ea3b3532e9: () => It, __wbg_url_7807f6a1fddc3e23: () => lt, __wbg_view_7f0ce470793a340f: () => ft, __wbg_window_c6fb939a7f436783: () => St, __wbindgen_cb_drop: () => rt, __wbindgen_closure_wrapper685: () => Zt, __wbindgen_debug_string: () => Qt, __wbindgen_is_function: () => Rt, __wbindgen_is_undefined: () => ot, __wbindgen_memory: () => Yt, __wbindgen_number_new: () => it, __wbindgen_object_clone_ref: () => _t, __wbindgen_object_drop_ref: () => tt, __wbindgen_string_get: () => nt, __wbindgen_string_new: () => et, __wbindgen_throw: () => Xt, fetch: () => q, getMemory: () => U });
var D = new WebAssembly.Instance($, { "./index_bg.js": b });
var r = D.exports;
function U() {
  return r.memory;
}
var l = new Array(128).fill(void 0);
l.push(void 0, null, true, false);
function o(t) {
  return l[t];
}
var m = l.length;
function W(t) {
  t < 132 || (l[t] = m, m = t);
}
function w(t) {
  let e = o(t);
  return W(t), e;
}
var v = typeof TextDecoder > "u" ? (0, module.require)("util").TextDecoder : TextDecoder;
var L = new v("utf-8", { ignoreBOM: true, fatal: true });
L.decode();
var x = null;
function F() {
  return (x === null || x.byteLength === 0) && (x = new Uint8Array(r.memory.buffer)), x;
}
function p(t, e) {
  return t = t >>> 0, L.decode(F().subarray(t, t + e));
}
function s(t) {
  m === l.length && l.push(l.length + 1);
  let e = m;
  return m = l[e], l[e] = t, e;
}
var y = 0;
var N = typeof TextEncoder > "u" ? (0, module.require)("util").TextEncoder : TextEncoder;
var R = new N("utf-8");
var P = typeof R.encodeInto == "function" ? function(t, e) {
  return R.encodeInto(t, e);
} : function(t, e) {
  let n = R.encode(t);
  return e.set(n), { read: t.length, written: n.length };
};
function z(t, e, n) {
  if (n === void 0) {
    let f = R.encode(t), h = e(f.length, 1) >>> 0;
    return F().subarray(h, h + f.length).set(f), y = f.length, h;
  }
  let _ = t.length, i = e(_, 1) >>> 0, u = F(), c = 0;
  for (; c < _; c++) {
    let f = t.charCodeAt(c);
    if (f > 127)
      break;
    u[i + c] = f;
  }
  if (c !== _) {
    c !== 0 && (t = t.slice(c)), i = n(i, _, _ = c + t.length * 3, 1) >>> 0;
    let f = F().subarray(i + c, i + _), h = P(t, f);
    c += h.written, i = n(i, _, c, 1) >>> 0;
  }
  return y = c, i;
}
function d(t) {
  return t == null;
}
var k = null;
function g() {
  return (k === null || k.byteLength === 0) && (k = new Int32Array(r.memory.buffer)), k;
}
function j(t) {
  let e = typeof t;
  if (e == "number" || e == "boolean" || t == null)
    return `${t}`;
  if (e == "string")
    return `"${t}"`;
  if (e == "symbol") {
    let i = t.description;
    return i == null ? "Symbol" : `Symbol(${i})`;
  }
  if (e == "function") {
    let i = t.name;
    return typeof i == "string" && i.length > 0 ? `Function(${i})` : "Function";
  }
  if (Array.isArray(t)) {
    let i = t.length, u = "[";
    i > 0 && (u += j(t[0]));
    for (let c = 1; c < i; c++)
      u += ", " + j(t[c]);
    return u += "]", u;
  }
  let n = /\[object ([^\]]+)\]/.exec(toString.call(t)), _;
  if (n.length > 1)
    _ = n[1];
  else
    return toString.call(t);
  if (_ == "Object")
    try {
      return "Object(" + JSON.stringify(t) + ")";
    } catch {
      return "Object";
    }
  return t instanceof Error ? `${t.name}: ${t.message}
${t.stack}` : _;
}
var A = typeof FinalizationRegistry > "u" ? { register: () => {
}, unregister: () => {
} } : new FinalizationRegistry((t) => {
  r.__wbindgen_export_2.get(t.dtor)(t.a, t.b);
});
function H(t, e, n, _) {
  let i = { a: t, b: e, cnt: 1, dtor: n }, u = (...c) => {
    i.cnt++;
    let f = i.a;
    i.a = 0;
    try {
      return _(f, i.b, ...c);
    } finally {
      --i.cnt === 0 ? (r.__wbindgen_export_2.get(i.dtor)(f, i.b), A.unregister(i)) : i.a = f;
    }
  };
  return u.original = i, A.register(u, i, i), u;
}
function B(t, e, n) {
  r._dyn_core__ops__function__FnMut__A____Output___R_as_wasm_bindgen__closure__WasmClosure___describe__invoke__h06753de439d9b845(t, e, s(n));
}
function q(t, e, n) {
  let _ = r.fetch(s(t), s(e), s(n));
  return w(_);
}
function a(t, e) {
  try {
    return t.apply(this, e);
  } catch (n) {
    r.__wbindgen_exn_store(s(n));
  }
}
function J(t, e, n, _) {
  r.wasm_bindgen__convert__closures__invoke2_mut__h5398cc53c04714db(t, e, s(n), s(_));
}
var V = Object.freeze({ Error: 0, 0: "Error", Follow: 1, 1: "Follow", Manual: 2, 2: "Manual" });
var G = Object.freeze({ Off: 0, 0: "Off", Lossy: 1, 1: "Lossy", Lossless: 2, 2: "Lossless" });
var K = typeof FinalizationRegistry > "u" ? { register: () => {
}, unregister: () => {
} } : new FinalizationRegistry((t) => r.__wbg_intounderlyingbytesource_free(t >>> 0));
var O = class {
  __destroy_into_raw() {
    let e = this.__wbg_ptr;
    return this.__wbg_ptr = 0, K.unregister(this), e;
  }
  free() {
    let e = this.__destroy_into_raw();
    r.__wbg_intounderlyingbytesource_free(e);
  }
  get type() {
    let e, n;
    try {
      let u = r.__wbindgen_add_to_stack_pointer(-16);
      r.intounderlyingbytesource_type(u, this.__wbg_ptr);
      var _ = g()[u / 4 + 0], i = g()[u / 4 + 1];
      return e = _, n = i, p(_, i);
    } finally {
      r.__wbindgen_add_to_stack_pointer(16), r.__wbindgen_free(e, n, 1);
    }
  }
  get autoAllocateChunkSize() {
    return r.intounderlyingbytesource_autoAllocateChunkSize(this.__wbg_ptr) >>> 0;
  }
  start(e) {
    r.intounderlyingbytesource_start(this.__wbg_ptr, s(e));
  }
  pull(e) {
    let n = r.intounderlyingbytesource_pull(this.__wbg_ptr, s(e));
    return w(n);
  }
  cancel() {
    let e = this.__destroy_into_raw();
    r.intounderlyingbytesource_cancel(e);
  }
};
var Q = typeof FinalizationRegistry > "u" ? { register: () => {
}, unregister: () => {
} } : new FinalizationRegistry((t) => r.__wbg_intounderlyingsink_free(t >>> 0));
var M = class {
  __destroy_into_raw() {
    let e = this.__wbg_ptr;
    return this.__wbg_ptr = 0, Q.unregister(this), e;
  }
  free() {
    let e = this.__destroy_into_raw();
    r.__wbg_intounderlyingsink_free(e);
  }
  write(e) {
    let n = r.intounderlyingsink_write(this.__wbg_ptr, s(e));
    return w(n);
  }
  close() {
    let e = this.__destroy_into_raw(), n = r.intounderlyingsink_close(e);
    return w(n);
  }
  abort(e) {
    let n = this.__destroy_into_raw(), _ = r.intounderlyingsink_abort(n, s(e));
    return w(_);
  }
};
var X = typeof FinalizationRegistry > "u" ? { register: () => {
}, unregister: () => {
} } : new FinalizationRegistry((t) => r.__wbg_intounderlyingsource_free(t >>> 0));
var E = class {
  __destroy_into_raw() {
    let e = this.__wbg_ptr;
    return this.__wbg_ptr = 0, X.unregister(this), e;
  }
  free() {
    let e = this.__destroy_into_raw();
    r.__wbg_intounderlyingsource_free(e);
  }
  pull(e) {
    let n = r.intounderlyingsource_pull(this.__wbg_ptr, s(e));
    return w(n);
  }
  cancel() {
    let e = this.__destroy_into_raw();
    r.intounderlyingsource_cancel(e);
  }
};
var Y = typeof FinalizationRegistry > "u" ? { register: () => {
}, unregister: () => {
} } : new FinalizationRegistry((t) => r.__wbg_minifyconfig_free(t >>> 0));
var S = class {
  __destroy_into_raw() {
    let e = this.__wbg_ptr;
    return this.__wbg_ptr = 0, Y.unregister(this), e;
  }
  free() {
    let e = this.__destroy_into_raw();
    r.__wbg_minifyconfig_free(e);
  }
  get js() {
    return r.__wbg_get_minifyconfig_js(this.__wbg_ptr) !== 0;
  }
  set js(e) {
    r.__wbg_set_minifyconfig_js(this.__wbg_ptr, e);
  }
  get html() {
    return r.__wbg_get_minifyconfig_html(this.__wbg_ptr) !== 0;
  }
  set html(e) {
    r.__wbg_set_minifyconfig_html(this.__wbg_ptr, e);
  }
  get css() {
    return r.__wbg_get_minifyconfig_css(this.__wbg_ptr) !== 0;
  }
  set css(e) {
    r.__wbg_set_minifyconfig_css(this.__wbg_ptr, e);
  }
};
var Z = typeof FinalizationRegistry > "u" ? { register: () => {
}, unregister: () => {
} } : new FinalizationRegistry((t) => r.__wbg_r2range_free(t >>> 0));
var T = class {
  __destroy_into_raw() {
    let e = this.__wbg_ptr;
    return this.__wbg_ptr = 0, Z.unregister(this), e;
  }
  free() {
    let e = this.__destroy_into_raw();
    r.__wbg_r2range_free(e);
  }
  get offset() {
    try {
      let _ = r.__wbindgen_add_to_stack_pointer(-16);
      r.__wbg_get_r2range_offset(_, this.__wbg_ptr);
      var e = g()[_ / 4 + 0], n = g()[_ / 4 + 1];
      return e === 0 ? void 0 : n >>> 0;
    } finally {
      r.__wbindgen_add_to_stack_pointer(16);
    }
  }
  set offset(e) {
    r.__wbg_set_r2range_offset(this.__wbg_ptr, !d(e), d(e) ? 0 : e);
  }
  get length() {
    try {
      let _ = r.__wbindgen_add_to_stack_pointer(-16);
      r.__wbg_get_r2range_length(_, this.__wbg_ptr);
      var e = g()[_ / 4 + 0], n = g()[_ / 4 + 1];
      return e === 0 ? void 0 : n >>> 0;
    } finally {
      r.__wbindgen_add_to_stack_pointer(16);
    }
  }
  set length(e) {
    r.__wbg_set_r2range_length(this.__wbg_ptr, !d(e), d(e) ? 0 : e);
  }
  get suffix() {
    try {
      let _ = r.__wbindgen_add_to_stack_pointer(-16);
      r.__wbg_get_r2range_suffix(_, this.__wbg_ptr);
      var e = g()[_ / 4 + 0], n = g()[_ / 4 + 1];
      return e === 0 ? void 0 : n >>> 0;
    } finally {
      r.__wbindgen_add_to_stack_pointer(16);
    }
  }
  set suffix(e) {
    r.__wbg_set_r2range_suffix(this.__wbg_ptr, !d(e), d(e) ? 0 : e);
  }
};
function tt(t) {
  w(t);
}
function et(t, e) {
  let n = p(t, e);
  return s(n);
}
function nt(t, e) {
  let n = o(e), _ = typeof n == "string" ? n : void 0;
  var i = d(_) ? 0 : z(_, r.__wbindgen_malloc, r.__wbindgen_realloc), u = y;
  g()[t / 4 + 1] = u, g()[t / 4 + 0] = i;
}
function rt(t) {
  let e = w(t).original;
  return e.cnt-- == 1 ? (e.a = 0, true) : false;
}
function _t(t) {
  let e = o(t);
  return s(e);
}
function ot(t) {
  return o(t) === void 0;
}
function it(t) {
  return s(t);
}
function st(t) {
  let e = o(t).cf;
  return d(e) ? 0 : s(e);
}
function ct(t) {
  let e = o(t).byobRequest;
  return d(e) ? 0 : s(e);
}
function ut() {
  return a(function(t) {
    o(t).close();
  }, arguments);
}
function ft(t) {
  let e = o(t).view;
  return d(e) ? 0 : s(e);
}
function gt() {
  return a(function(t, e) {
    o(t).respond(e >>> 0);
  }, arguments);
}
function at() {
  return a(function(t) {
    o(t).close();
  }, arguments);
}
function bt() {
  return a(function(t, e) {
    o(t).enqueue(o(e));
  }, arguments);
}
function dt(t, e) {
  let n = o(e).method, _ = z(n, r.__wbindgen_malloc, r.__wbindgen_realloc), i = y;
  g()[t / 4 + 1] = i, g()[t / 4 + 0] = _;
}
function lt(t, e) {
  let n = o(e).url, _ = z(n, r.__wbindgen_malloc, r.__wbindgen_realloc), i = y;
  g()[t / 4 + 1] = i, g()[t / 4 + 0] = _;
}
function wt(t) {
  let e = o(t).headers;
  return s(e);
}
function pt() {
  return a(function() {
    let t = new Headers();
    return s(t);
  }, arguments);
}
function yt() {
  return a(function(t, e, n, _, i) {
    o(t).set(p(e, n), p(_, i));
  }, arguments);
}
function ht(t) {
  console.error(o(t));
}
function mt() {
  return a(function(t, e) {
    let n = new Response(o(t), o(e));
    return s(n);
  }, arguments);
}
function xt() {
  return a(function(t, e, n) {
    let _ = new Response(t === 0 ? void 0 : p(t, e), o(n));
    return s(_);
  }, arguments);
}
function kt() {
  return a(function(t, e) {
    let n = new Response(o(t), o(e));
    return s(n);
  }, arguments);
}
function Ft(t) {
  let e = o(t).queueMicrotask;
  return s(e);
}
function Rt(t) {
  return typeof o(t) == "function";
}
function zt(t) {
  queueMicrotask(o(t));
}
function jt(t, e) {
  let n = new Function(p(t, e));
  return s(n);
}
function Ot() {
  return a(function(t, e) {
    let n = o(t).call(o(e));
    return s(n);
  }, arguments);
}
function Mt() {
  let t = new Object();
  return s(t);
}
function Et() {
  return a(function() {
    let t = self.self;
    return s(t);
  }, arguments);
}
function St() {
  return a(function() {
    let t = window.window;
    return s(t);
  }, arguments);
}
function Tt() {
  return a(function() {
    let t = globalThis.globalThis;
    return s(t);
  }, arguments);
}
function qt() {
  return a(function() {
    let t = global.global;
    return s(t);
  }, arguments);
}
function At(t) {
  let e;
  try {
    e = o(t) instanceof Error;
  } catch {
    e = false;
  }
  return e;
}
function Lt(t, e) {
  let n = new Error(p(t, e));
  return s(n);
}
function Ct(t) {
  let e = o(t).cause;
  return s(e);
}
function It(t) {
  let e = o(t).toString();
  return s(e);
}
function $t() {
  return a(function(t, e, n) {
    let _ = o(t).call(o(e), o(n));
    return s(_);
  }, arguments);
}
function Dt(t, e) {
  try {
    var n = { a: t, b: e }, _ = (u, c) => {
      let f = n.a;
      n.a = 0;
      try {
        return J(f, n.b, u, c);
      } finally {
        n.a = f;
      }
    };
    let i = new Promise(_);
    return s(i);
  } finally {
    n.a = n.b = 0;
  }
}
function Ut(t) {
  let e = Promise.resolve(o(t));
  return s(e);
}
function Wt(t, e) {
  let n = o(t).then(o(e));
  return s(n);
}
function vt(t) {
  let e = o(t).buffer;
  return s(e);
}
function Nt(t, e, n) {
  let _ = new Uint8Array(o(t), e >>> 0, n >>> 0);
  return s(_);
}
function Pt(t, e, n) {
  o(t).set(o(e), n >>> 0);
}
function Ht(t) {
  return o(t).length;
}
function Bt(t) {
  let e = new Uint8Array(t >>> 0);
  return s(e);
}
function Jt(t) {
  let e = o(t).buffer;
  return s(e);
}
function Vt(t) {
  return o(t).byteLength;
}
function Gt(t) {
  return o(t).byteOffset;
}
function Kt() {
  return a(function(t, e, n) {
    return Reflect.set(o(t), o(e), o(n));
  }, arguments);
}
function Qt(t, e) {
  let n = j(o(e)), _ = z(n, r.__wbindgen_malloc, r.__wbindgen_realloc), i = y;
  g()[t / 4 + 1] = i, g()[t / 4 + 0] = _;
}
function Xt(t, e) {
  throw new Error(p(t, e));
}
function Yt() {
  let t = r.memory;
  return s(t);
}
function Zt(t, e, n) {
  let _ = H(t, e, 105, B);
  return s(_);
}
var ie = { fetch: q, scheduled: void 0, queue: void 0 };

// ../../.npm/_npx/32026684e21afda6/node_modules/wrangler/templates/middleware/middleware-ensure-req-body-drained.ts
var drainBody = async (request, env, _ctx, middlewareCtx) => {
  try {
    return await middlewareCtx.next(request, env);
  } finally {
    try {
      if (request.body !== null && !request.bodyUsed) {
        const reader = request.body.getReader();
        while (!(await reader.read()).done) {
        }
      }
    } catch (e) {
      console.error("Failed to drain the unused request body.", e);
    }
  }
};
var middleware_ensure_req_body_drained_default = drainBody;
var wrap = void 0;

// ../../.npm/_npx/32026684e21afda6/node_modules/wrangler/templates/middleware/middleware-miniflare3-json-error.ts
function reduceError(e) {
  return {
    name: e?.name,
    message: e?.message ?? String(e),
    stack: e?.stack,
    cause: e?.cause === void 0 ? void 0 : reduceError(e.cause)
  };
}
var jsonError = async (request, env, _ctx, middlewareCtx) => {
  try {
    return await middlewareCtx.next(request, env);
  } catch (e) {
    const error = reduceError(e);
    return Response.json(error, {
      status: 500,
      headers: { "MF-Experimental-Error-Stack": "true" }
    });
  }
};
var middleware_miniflare3_json_error_default = jsonError;
var wrap2 = void 0;

// .wrangler/tmp/bundle-WPCyc0/middleware-insertion-facade.js
var envWrappers = [wrap, wrap2].filter(Boolean);
var facade = {
  ...ie,
  envWrappers,
  middleware: [
    middleware_ensure_req_body_drained_default,
    middleware_miniflare3_json_error_default,
    ...ie.middleware ? ie.middleware : []
  ].filter(Boolean)
};
var middleware_insertion_facade_default = facade;

// ../../.npm/_npx/32026684e21afda6/node_modules/wrangler/templates/middleware/common.ts
var __facade_middleware__ = [];
function __facade_register__(...args) {
  __facade_middleware__.push(...args.flat());
}
function __facade_invokeChain__(request, env, ctx, dispatch, middlewareChain) {
  const [head, ...tail] = middlewareChain;
  const middlewareCtx = {
    dispatch,
    next(newRequest, newEnv) {
      return __facade_invokeChain__(newRequest, newEnv, ctx, dispatch, tail);
    }
  };
  return head(request, env, ctx, middlewareCtx);
}
function __facade_invoke__(request, env, ctx, dispatch, finalMiddleware) {
  return __facade_invokeChain__(request, env, ctx, dispatch, [
    ...__facade_middleware__,
    finalMiddleware
  ]);
}

// .wrangler/tmp/bundle-WPCyc0/middleware-loader.entry.ts
var __Facade_ScheduledController__ = class {
  constructor(scheduledTime, cron, noRetry) {
    this.scheduledTime = scheduledTime;
    this.cron = cron;
    this.#noRetry = noRetry;
  }
  #noRetry;
  noRetry() {
    if (!(this instanceof __Facade_ScheduledController__)) {
      throw new TypeError("Illegal invocation");
    }
    this.#noRetry();
  }
};
var __facade_modules_fetch__ = function(request, env, ctx) {
  if (middleware_insertion_facade_default.fetch === void 0)
    throw new Error("Handler does not export a fetch() function.");
  return middleware_insertion_facade_default.fetch(request, env, ctx);
};
function getMaskedEnv(rawEnv) {
  let env = rawEnv;
  if (middleware_insertion_facade_default.envWrappers && middleware_insertion_facade_default.envWrappers.length > 0) {
    for (const wrapFn of middleware_insertion_facade_default.envWrappers) {
      env = wrapFn(env);
    }
  }
  return env;
}
var registeredMiddleware = false;
var facade2 = {
  ...middleware_insertion_facade_default.tail && {
    tail: maskHandlerEnv(middleware_insertion_facade_default.tail)
  },
  ...middleware_insertion_facade_default.trace && {
    trace: maskHandlerEnv(middleware_insertion_facade_default.trace)
  },
  ...middleware_insertion_facade_default.scheduled && {
    scheduled: maskHandlerEnv(middleware_insertion_facade_default.scheduled)
  },
  ...middleware_insertion_facade_default.queue && {
    queue: maskHandlerEnv(middleware_insertion_facade_default.queue)
  },
  ...middleware_insertion_facade_default.test && {
    test: maskHandlerEnv(middleware_insertion_facade_default.test)
  },
  ...middleware_insertion_facade_default.email && {
    email: maskHandlerEnv(middleware_insertion_facade_default.email)
  },
  fetch(request, rawEnv, ctx) {
    const env = getMaskedEnv(rawEnv);
    if (middleware_insertion_facade_default.middleware && middleware_insertion_facade_default.middleware.length > 0) {
      if (!registeredMiddleware) {
        registeredMiddleware = true;
        for (const middleware of middleware_insertion_facade_default.middleware) {
          __facade_register__(middleware);
        }
      }
      const __facade_modules_dispatch__ = function(type, init) {
        if (type === "scheduled" && middleware_insertion_facade_default.scheduled !== void 0) {
          const controller = new __Facade_ScheduledController__(
            Date.now(),
            init.cron ?? "",
            () => {
            }
          );
          return middleware_insertion_facade_default.scheduled(controller, env, ctx);
        }
      };
      return __facade_invoke__(
        request,
        env,
        ctx,
        __facade_modules_dispatch__,
        __facade_modules_fetch__
      );
    } else {
      return __facade_modules_fetch__(request, env, ctx);
    }
  }
};
function maskHandlerEnv(handler) {
  return (data, env, ctx) => handler(data, getMaskedEnv(env), ctx);
}
var middleware_loader_entry_default = facade2;
export {
  O as IntoUnderlyingByteSource,
  M as IntoUnderlyingSink,
  E as IntoUnderlyingSource,
  S as MinifyConfig,
  G as PolishConfig,
  T as R2Range,
  V as RequestRedirect,
  vt as __wbg_buffer_12d079cc21e14bdb,
  Jt as __wbg_buffer_dd7f74bc60f1faab,
  ct as __wbg_byobRequest_72fca99f9c32c193,
  Vt as __wbg_byteLength_58f7b4fab1919d44,
  Gt as __wbg_byteOffset_81d60f7392524f62,
  Ot as __wbg_call_27c0f87801dedf93,
  $t as __wbg_call_b3ca7c6051f9bec1,
  Ct as __wbg_cause_3d9c85ebaf6b1155,
  st as __wbg_cf_c62699e22b0d5280,
  ut as __wbg_close_184931724d961ccc,
  at as __wbg_close_a994f9425dab445c,
  bt as __wbg_enqueue_ea194723156c0cc2,
  ht as __wbg_error_8e3928cfb8a43e2b,
  Tt as __wbg_globalThis_d1e6af4856ba331b,
  qt as __wbg_global_207b558942527489,
  wt as __wbg_headers_abb199c3be8d817c,
  At as __wbg_instanceof_Error_e20bb56fd5591a93,
  Ht as __wbg_length_c20a40f15020d68a,
  dt as __wbg_method_83327ed2e3f3229c,
  Lt as __wbg_new_28c511d9baebfa89,
  Mt as __wbg_new_72fb9a18b5ae2624,
  Dt as __wbg_new_81740750da40724f,
  pt as __wbg_new_ab6fd82b10560829,
  jt as __wbg_newnoargs_e258087cd0daa0ea,
  Nt as __wbg_newwithbyteoffsetandlength_aa4a17c33a06e5cb,
  Bt as __wbg_newwithlength_e9b4878cebadb3d3,
  mt as __wbg_newwithoptbuffersourceandinit_a4fa81e77259bb96,
  kt as __wbg_newwithoptreadablestreamandinit_0b825f969ca543d6,
  xt as __wbg_newwithoptstrandinit_219732174c595a25,
  Ft as __wbg_queueMicrotask_3cbae2ec6b6cd3d6,
  zt as __wbg_queueMicrotask_481971b0d87f3dd4,
  Ut as __wbg_resolve_b0083a7967828ec8,
  gt as __wbg_respond_b1a43b2e3a06d525,
  Et as __wbg_self_ce0dbfc45cf2f5be,
  Kt as __wbg_set_1f9b04f170055d33,
  Pt as __wbg_set_a47bac70306a19a7,
  yt as __wbg_set_cb0e7a5c2dd66afd,
  Wt as __wbg_then_0c86a60e8fcfe9f6,
  It as __wbg_toString_ffe4c9ea3b3532e9,
  lt as __wbg_url_7807f6a1fddc3e23,
  ft as __wbg_view_7f0ce470793a340f,
  St as __wbg_window_c6fb939a7f436783,
  rt as __wbindgen_cb_drop,
  Zt as __wbindgen_closure_wrapper685,
  Qt as __wbindgen_debug_string,
  Rt as __wbindgen_is_function,
  ot as __wbindgen_is_undefined,
  Yt as __wbindgen_memory,
  it as __wbindgen_number_new,
  _t as __wbindgen_object_clone_ref,
  tt as __wbindgen_object_drop_ref,
  nt as __wbindgen_string_get,
  et as __wbindgen_string_new,
  Xt as __wbindgen_throw,
  middleware_loader_entry_default as default,
  q as fetch,
  U as getMemory,
  oe as wasmModule
};
//# sourceMappingURL=shim.js.map
