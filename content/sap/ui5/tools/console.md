---
title: ShowConsole.js
---

```js
// ==UserScript==
// @name         ShowConsole
// @namespace    http://tampermonkey.net/
// @version      0.1
// @description  Display SSSConsole!
// @author       aemre
// @match        http://*/*
// @match        https://*/*
// @grant        none
// ==/UserScript==


(function () {
  'use strict';

  const script = document.body.appendChild(document.createElement("script"));
  script.onload = main;
  script.src = "https://cdnjs.cloudflare.com/ajax/libs/rxjs/6.3.3/rxjs.umd.min.js";



  function main() {
    const sap = window.sap;
    const rxjs = window.rxjs;

    const { from, fromEvent, Subject, timer } = rxjs;
    const { concatMap, debounceTime, filter, first, map, mapTo, pluck, sample, sequenceEqual, switchMap, takeUntil, tap } = rxjs.operators;
    const containerId = "sssconsole";

    const move$ = fromEvent(document, 'mousemove');
    const down$ = fromEvent(document, 'mousedown');
    const up$ = fromEvent(document, 'mouseup');
    const click$ = fromEvent(document, "click");
    const keydown$ = fromEvent(document, "keydown");
    const keyup$ = fromEvent(document, "keyup");

    const command$ = new Subject();
    const commandResult$ = new Subject();
    let selectSubscription;


    const extensionCommands = {
      sap: function (args) {
        const sapstatic = document.getElementById("sap-ui-static");

        switch (args._[0]) {
          case "show":
            if (sapstatic) {
              sapstatic.style.display = "block";
            }
            break;
          case "hide":
            if (sapstatic) {
              sapstatic.style.display = "none";
            }
            break;
          case "select":
            {
              const clearConsole = args.clear;
              const runMethod = args.run;
              const runParams = args.p ? JSON.parse(args.p) : undefined;
              const trigger$ = move$.pipe(sample(keyup$.pipe(pluck("code"), filter(c => ["ControlRight", "MetaRight"].includes(c)))));
              window.getDetail = getDetail; //expose to public
              window.printDetail = printDetail;
              if (selectSubscription) selectSubscription.unsubscribe();
              selectSubscription = trigger$.pipe(
                pluck("path"),
                concatMap(e => from(e).pipe(
                  map(a => a.getAttribute && a.getAttribute("data-sap-ui")),
                  filter(Boolean),
                  first(),
                )),
              ).subscribe(printDetail);

              function printDetail(idOrControl) {
                const control = typeof idOrControl === "string" ? sap.ui.getCore().byId(idOrControl) : idOrControl;
                const models = Object.assign({}, control.oModels, control.oPropagatedProperties.oModels);
                let result = getDetail(control);
                let firstResult = result;
                let allSelectors = Object.create(control);
                allSelectors.push = Array.prototype.push.bind(allSelectors);
                allSelectors.push(control);
                let funcBodyMap;

                if (clearConsole) {
                  console.clear();
                }
                console.group("%cType:", "font-weight: bold", result.type);
                console.log("s[0] Id: " + result.id);

                if (!isEmpty(result.contexts)) {
                  console.table(result.contexts);
                }

                if (!isEmpty(result.events)) {
                  console.table(result.events);
                }

                if (!isEmpty(result.data)) {
                  console.log("Custom data:");
                  console.table(result.data);
                }
                funcBodyMap = result.funcBody;

                let debugInfo = result.debugInfo;

                let view;
                while (result.parent) {
                  result = getDetail(result.parent, true);
                  funcBodyMap = Object.assign({}, result.funcBody, funcBodyMap);
                  if (result.hasValuableContent) {
                    allSelectors.push(result.control);
                    console.groupCollapsed("%cType:", "font-weight: bold", result.type);
                    console.log(`s[${allSelectors.length - 1}] Id: ${result.id}`);

                    if (!isEmpty(result.contexts)) {
                      console.table(result.contexts);
                    }

                    if (!isEmpty(result.events)) {
                      console.table(result.events);
                    }

                    if (!isEmpty(result.data)) {
                      console.log("Custom data:");
                      console.table(result.data);
                    }
                    console.groupEnd();
                  } else {
                    console.log("%cType:", "font-weight: bold; color: lightgray", result.type);
                  }
                  if (!view && result.parent instanceof sap.ui.core.mvc.View) {
                    view = result.parent;
                  }
                  debugInfo = Object.assign({}, result.debugInfo, debugInfo);
                }
                Object.assign(control, funcBodyMap);

                if (view) {
                  console.log("%cView:", "font-weight: bold", view.getViewName());
                  console.log("%cController:", "font-weight: bold", view.getControllerName());
                }

                Object.entries(models).map(function replaceWithData([name, model]) {
                  var data = {};
                  if (model && model.getData) {
                    data = model.getData();
                  }
                  models[name] = Object.create(data, {
                    _model: {
                      value: model
                    }
                  });
                });

                window.m = models;
                window.mx = firstResult.modelContexts;
                window.s = allSelectors;
                window.v = view;
                window.c = view && view.getController();
                window.o = window.c && window.c.getOwnerComponent && window.c.getOwnerComponent();
                window.byId = id => view && view.byId(id) || sap.ui.getCore().byId(id);

                if (!isEmpty(debugInfo)) {
                  console.group("Debug Info");
                  console.table(debugInfo);
                  console.groupEnd("Debug Info");
                }

                console.groupCollapsed("Helper Variables");
                console.log("%cs:", "font-weight: bold", "Selected Control");
                console.log("%cm:", "font-weight: bold", "Available Models of selected control");
                console.log("%cmx:", "font-weight: bold", "Models contexts of selected control");
                console.log("%cv:", "font-weight: bold", "View");
                console.log("%cc:", "font-weight: bold", "Controller");
                console.log("%co:", "font-weight: bold", "Owner Component");
                console.log("%cbyId:", "font-weight: bold", "Selector within view or global scope");
                console.groupEnd("Helper Variables");

                console.groupEnd();

                if (runMethod && typeof window.s[runMethod] === "function") {
                  window.s[runMethod](runParams);
                }
              }

              function getDetail(control, isRecursiveCall) {
                const result = {
                  control,
                  id: control.getId(),
                  type: control.getMetadata().getName(),
                  contexts: {},
                  events: {},
                  funcBody: {},
                  parent: control.getParent && control.getParent(),
                  models: Object.assign({}, control.oModels, control.oPropagatedProperties.oModels),
                  modelContexts: {},
                  hasValuableContent: false,
                  debugInfo: {},
                }

                // List Bindings
                Object.entries(control.mBindingInfos).forEach(function ([attribute, value]) {
                  const binding = value.binding;
                  if (!binding) {
                    return;
                  }
                  let bindingContext = binding.oContext;
                  if (binding.getValue) {
                    let values = binding.getValue();
                    try {
                        if (typeof values === "object") {
                          values = JSON.stringify(values);
                        }
                    } catch (e) {
                    }
                    result.contexts[attribute] = {
                      path: value.parts.map(a => a.model ? `{${a.model}>${a.path}}` : `{${a.path}}`).join(", "),
                      values,
                      contextPath: bindingContext && bindingContext.getPath(),
                    }
                  } else if (binding.getLength) {
                    result.contexts[attribute] = {
                      path: value.model ? `{${value.model}>${value.path}}` : `{${value.path}}`,
                      values: `[[${binding.getLength()} items]]`,
                      contextPath: bindingContext && bindingContext.getPath(),
                    }
                  }
                });

                // List Events
                Object.entries(control.mEventRegistry).map(function getEvents([name, fncList]) {
                  return {
                    name,
                    functions: fncList.map(f => f.fFunction.name).filter(Boolean).join(","),
                    funcBody: fncList.map(f => f.fFunction).pop(),
                  }
                })
                  .filter(i => i.functions)
                  .forEach(function (item) {
                    result.events[item.name] = item.functions;
                    result.funcBody[item.name] = item.funcBody;
                  });

                result.data = control.data && control.data();

                result.hasValuableContent = !isRecursiveCall || !isEmpty(result.binding) || !isEmpty(result.events);

                const debugInfo = control.getBinding("debugInfo") || control.getBinding("/debugInfo");
                if (debugInfo && debugInfo.getValue) {
                  let value = debugInfo.getValue();
                  if (typeof value === "string") {
                    value = JSON.parse(value);
                  }
                  result.debugInfo = value;
                }

                if (result.models) {
                  Object.keys(result.models).forEach(function getContextObject(key) {
                    const modelName = key === "undefined" ? undefined : key;
                    const modelContext = result.control.getBindingContext(modelName);
                    if (modelContext && modelContext.getObject) {
                      result.modelContexts[modelName] = modelContext.getObject();
                    }
                  });
                }

                return result;
              }
            }
            break;

          case "pdf":
            {

              const e = document.location.pathname,
                n = e.substring(0, e.indexOf("index.html")) + "xml/topic",
                t = document.querySelector("#progressIndicator");
              if (t) {
                var r = parseInt(t.innerText.split(" ")[1])
                  , i = 0;
                window.incrementCounter = function () {
                  ++i == r && print()
                }
                  ;
                for (var o = 1; o <= r; o++) {
                  document.write("<img src='" + n + o + ".svg' onload='incrementCounter()'/>")
                }
              } else {
                alert("Sayfa desteklenmiyor.");
              }
              return true;

            }
            break;
          default:
            return false;
        }

        return true;
      },
      history: function (args) {
        switch (args._[0]) {
          case "clear":
            localStorage.removeItem(containerId);
            return true;
        }
      },
      exit: function () {
        let form = document.getElementById(containerId);
        if (form) {
          form.parentNode.removeChild(form);
        }
      },
      test: function (args) {
        console.log({ args });
        return true;
      },
      libs: function () {
        const libs = [
          "lodash-fp/0.10.4/lodash-fp.js",
          "dexie/2.0.4/dexie.js",
          "moment.js/2.24.0/moment.js",
        ].map(fullUrl).forEach(loadLibrary);

        function fullUrl(lib) {
          return "https://cdnjs.cloudflare.com/ajax/libs/" + lib;
        }
        function loadLibrary(url) {
          document.body.appendChild(document.createElement("script")).src = url
        }

        window.of = rxjs.of;
        window.from = rxjs.from;
        window.fromEvent = rxjs.fromEvent;
        window.interval = rxjs.interval;
        window.Subject = rxjs.Subject;
        window.timer = rxjs.timer;

        window.o = rxjs.operators;
        window.move$ = move$;
        window.down$ = down$;
        window.up$ = up$;
        window.click$ = click$;
        window.keydown$ = keydown$;
        window.keyup$ = keyup$;
        return true;
      },
    };




    var PASS_CODE = from("SSS".split('').map(a => 'Key' + a));

    keyup$.pipe(
      pluck("code"),
      bufferDebounceTime(300),
      switchMap(arr => from(arr).pipe(sequenceEqual(PASS_CODE))),
      filter(Boolean)
    ).subscribe(createInput);

    command$.subscribe(function (string) {
      const commandString = string.trim();
      if (!commandString) {
        return;
      }
      const parsed = parseCommand(commandString);
      const mergedCommands = Object.assign({}, window.sssApi, extensionCommands);
      const currCommand = mergedCommands[parsed.__cmd__];

      if (currCommand) {
        Promise.resolve().then(function runAsync() {
          commandResult$.next(currCommand(parsed));
        });
      } else {
        console.warn("Unknown command", parsed.__cmd__);
      }
    });

    commandResult$.subscribe(function (result) {
    });


    function bufferDebounceTime(time) {
      return (source) => {
        let bufferedValues = [];

        return source.pipe(
          tap(value => bufferedValues.push(value)),
          debounceTime(time),
          map(() => bufferedValues),
          tap(() => { bufferedValues = [] }),
        );
      };
    }


    function createInput() {
      let form = document.getElementById(containerId);
      let history = JSON.parse(localStorage.getItem(containerId) || "[]");
      let historyTmp = "";
      let historyIndex = history.length;

      if (!form) {

        form = document.body.appendChild(document.createElement("form"));
        form.id = containerId;

        const input = form.appendChild(document.createElement("input"));
        input.spellcheck = false;

        style(input, {
          borderRadius: "8px",
          border: "1px solid rgb(172, 172, 218)",
          padding: "8px",
          width: "220px",
          height: "17px",
          fontSize: "11px",
          fontFamily: "cursive",
          textShadow: "0 0 0px black",
          boxShadow: "0 0 12px 1px black",
          backgroundColor: "#eeeeee",
          outline: "none",
          boxSizing: "border-box",
        });


        style(form, {
          position: "fixed",
          margin: "10px auto",
          top: "5%",
          width: "100%",
          textAlign: "center",
          zIndex: "999999",
        });

        fromEvent(form, "submit").pipe(
          tap(e => e.preventDefault()),
          mapTo(input),
          pluck("value"),
          tap(function saveHistory(cmd) {
            if (cmd[0] === " ") {
              // If starts with space, then don't save history
              return;
            }
            cmd = cmd.trim();
            const historyIndex = history.findIndex(h => h === cmd);
            if (historyIndex !== -1) {
              history.splice(historyIndex, 1);
            }
            history.push(cmd);
            localStorage.setItem(containerId, JSON.stringify(history));
          }),
        ).subscribe(command$)


        keyup$.pipe(
          pluck("code")
        ).subscribe(function (key) {
          let nextHistoryIndex = history.length;

          switch (key) {
            case "Escape":
              form.style.display = "none";
              break;
            case "ArrowUp":
              nextHistoryIndex = historyIndex > 0 ? historyIndex - 1 : 0;
              break;
            case "ArrowDown":
              nextHistoryIndex = historyIndex < history.length ? historyIndex + 1 : history.length;
              break;
            case "ArrowLeft":
            case "ArrowRight":
              nextHistoryIndex = historyIndex;
              break;
            default:
              historyIndex = history.length;
          }

          if (nextHistoryIndex < history.length) {
            input.value = history[nextHistoryIndex];
          } else if (historyIndex !== history.length) {
            input.value = historyTmp;
          } else {
            historyTmp = input.value;
          }
          historyIndex = nextHistoryIndex;
        });

        commandResult$.subscribe(function (result) {
          if (result === true || result && result.hide === true) {
            document.getElementById(containerId).style.display = "none";
          }
          if (result === true || result && result.clear === true) {
            input.value = "";
          }
          if (typeof result === "object") {
            if (result.log) {
              console.log(result.log);
            }
            if (result.warn) {
              console.warn(result.warn);
            }
            if (result.error) {
              console.error(result.error);
            }
          }
          // read history after commands modify it manually
          history = JSON.parse(localStorage.getItem(containerId) || "[]");
        });

      } else {
        form.style.display = "block";
      }
      form.querySelector("input").focus();

      function style(element, attributes) {
        Object.entries(attributes).forEach(([attr, value]) => { element.style[attr] = value })
      }
    }

    if (window.sap && window.sap.ui && window.sap.ui.core) {
      command$.next("sap select --clear");
    }

    window.sss = function (sString) {
      command$.next(sString);
    }
  };



  function isEmpty(obj) {
    return !obj || (typeof obj === "object" && (obj.length === 0 || Object.entries(obj).length === 0));
  }
  function loadScript(src) {
    return new Promise(function (resolve, reject) {
      let script = document.createElement('script');
      script.src = src;

      script.onload = () => resolve(script);
      script.onerror = () => reject(new Error(`Script load error for ${src}`));

      document.head.append(script);
    });
  }

  function parseCommand(string) {
    const parsed = minimist(parseArgsStringToArgv(string));
    parsed.__cmd__ = parsed._.splice(0, 1)[0];
    return parsed;
    function parseArgsStringToArgv(n) { const t = /([^\s'"]([^\s'"]*(['"])([^\3]*?)\3)+[^\s'"]*)|[^\s'"]+|(['"])([^\5]*?)\5/gi, e = n, r = []; let s; do { null !== (s = t.exec(e)) && r.push(l(s[1], s[6], s[0])) } while (null !== s); return r; function l(...n) { for (let t = 0; t < n.length; t++) { const e = n[t]; if ("string" == typeof e) return e } } }
    function minimist(n, t) { t || (t = {}); var o = { bools: {}, strings: {}, unknownFn: null }; "function" == typeof t.unknown && (o.unknownFn = t.unknown), "boolean" == typeof t.boolean && t.boolean ? o.allBools = !0 : [].concat(t.boolean).filter(Boolean).forEach(function (n) { o.bools[n] = !0 }); var e = {}; Object.keys(t.alias || {}).forEach(function (n) { e[n] = [].concat(t.alias[n]), e[n].forEach(function (t) { e[t] = [n].concat(e[n].filter(function (n) { return t !== n })) }) }), [].concat(t.string).filter(Boolean).forEach(function (n) { o.strings[n] = !0, e[n] && (o.strings[e[n]] = !0) }); var s = t.default || {}, i = { _: [] }; Object.keys(o.bools).forEach(function (n) { a(n, void 0 !== s[n] && s[n]) }); var r = []; function a(n, t, s) { if (!s || !o.unknownFn || function (n, t) { return o.allBools && /^--[^=]+$/.test(t) || o.strings[n] || o.bools[n] || e[n] }(n, s) || !1 !== o.unknownFn(s)) { var r = !o.strings[n] && E(t) ? Number(t) : t; l(i, n.split("."), r), (e[n] || []).forEach(function (n) { l(i, n.split("."), r) }) } } function l(n, t, e) { var s = n; t.slice(0, -1).forEach(function (n) { void 0 === s[n] && (s[n] = {}), s = s[n] }); var i = t[t.length - 1]; void 0 === s[i] || o.bools[i] || "boolean" == typeof s[i] ? s[i] = e : Array.isArray(s[i]) ? s[i].push(e) : s[i] = [s[i], e] } function f(n) { return e[n].some(function (n) { return o.bools[n] }) } -1 !== n.indexOf("--") && (r = n.slice(n.indexOf("--") + 1), n = n.slice(0, n.indexOf("--"))); for (var c = 0; c < n.length; c++) { var u = n[c]; if (/^--.+=/.test(u)) { var h = u.match(/^--([^=]+)=([\s\S]*)$/), b = h[1], p = h[2]; o.bools[b] && (p = "false" !== p), a(b, p, u) } else if (/^--no-.+/.test(u)) { a(b = u.match(/^--no-(.+)/)[1], !1, u) } else if (/^--.+/.test(u)) { b = u.match(/^--(.+)/)[1]; void 0 === (g = n[c + 1]) || /^-/.test(g) || o.bools[b] || o.allBools || e[b] && f(b) ? /^(true|false)$/.test(g) ? (a(b, "true" === g, u), c++) : a(b, !o.strings[b] || "", u) : (a(b, g, u), c++) } else if (/^-[^-]+/.test(u)) { for (var v = u.slice(1, -1).split(""), d = !1, k = 0; k < v.length; k++) { var g; if ("-" !== (g = u.slice(k + 2))) { if (/[A-Za-z]/.test(v[k]) && /=/.test(g)) { a(v[k], g.split("=")[1], u), d = !0; break } if (/[A-Za-z]/.test(v[k]) && /-?\d+(\.\d*)?(e-?\d+)?$/.test(g)) { a(v[k], g, u), d = !0; break } if (v[k + 1] && v[k + 1].match(/\W/)) { a(v[k], u.slice(k + 2), u), d = !0; break } a(v[k], !o.strings[v[k]] || "", u) } else a(v[k], g, u) } b = u.slice(-1)[0]; d || "-" === b || (!n[c + 1] || /^(-|--)[^-]/.test(n[c + 1]) || o.bools[b] || e[b] && f(b) ? n[c + 1] && /true|false/.test(n[c + 1]) ? (a(b, "true" === n[c + 1], u), c++) : a(b, !o.strings[b] || "", u) : (a(b, n[c + 1], u), c++)) } else if (o.unknownFn && !1 === o.unknownFn(u) || i._.push(o.strings._ || !E(u) ? u : Number(u)), t.stopEarly) { i._.push.apply(i._, n.slice(c + 1)); break } } return Object.keys(s).forEach(function (n) { var t, o, r; t = i, o = n.split("."), r = t, o.slice(0, -1).forEach(function (n) { r = r[n] || {} }), o[o.length - 1] in r || (l(i, n.split("."), s[n]), (e[n] || []).forEach(function (t) { l(i, t.split("."), s[n]) })) }), t["--"] ? (i["--"] = new Array, r.forEach(function (n) { i["--"].push(n) })) : r.forEach(function (n) { i._.push(n) }), i; function E(n) { return "number" == typeof n || (!!/^0x[0-9a-f]+$/i.test(n) || /^[-+]?(?:\d+(?:\.\d*)?|\.\d+)(e[-+]?\d+)?$/.test(n)) } }
  }
})();
```
