---
title: Utility functions for SAP UI5
---

# Utility functions for SAP UI5

```js
sap.ui.define([
  "../library",
  "sap/ui/model/Filter",
  "sap/ui/model/FilterOperator",
  "sap/ui/model/FilterType",
  "sap/m/MessageBox",
], function (library, Filter, FilterOperator, FilterType, MessageBox) {
  "use strict";

  var Utils = {

      /**
       * Create function to sort array ascending
       */
      asc: function(sKey) {
          return function(a, b) {
              return Utils.result(sKey)(a) < Utils.result(sKey)(b) ? -1 : 1;
          };
      },

      /**
       * Create function to sort array descending
       */
      desc: function(sKey) {
          return function(a, b) {
              return Utils.result(sKey)(a) > Utils.result(sKey)(b) ? -1 : 1;
          };
      },

      /**
       * Show confirmation popup like: Utils.ask("Are you sure?").then(....)
       */
      ask: function(sQuestion, sType, oConfig) {
          return new Promise(function (resolve, reject) {
              var sPopupType = sType || "confirm";
              var oFinalConfig = Object.assign({}, oConfig, {
                  onClose: function(sAction) {
                      if ([MessageBox.Action.OK, MessageBox.Action.Close].includes(sAction)) {
                          resolve(true);
                      } else {
                          resolve(false);
                      }
                  }
              });
              MessageBox[sPopupType](sQuestion, oFinalConfig);
          });
      },

      /**
       * Usage:
       *  var type1s = [{type:1},{type:1},{type:2}]
       *                      .filter(Utils.byKey("type", 1));
       */
      byKey: function(sKey, value) {
          return function(item) {
              return Utils.result(sKey)(item) === value;
          };
      },


      /**
       * Usage:
       *  var type1_2s = [{type:1},{type:1},{type:2},{type:3}]
       *                      .filter(Utils.byKey("type", [1, 2]));
       */
      byKeys: function(sKey, aValues) {
          return function(item) {
              return aValues.includes(Utils.result(sKey)(item));
          };
      },

      byValue: function(value) {
          return function(item) {
              return item === value;
          };
      },

      byValues: function(aValues) {
          return function(item) {
              return aValues.includes(item);
          };
      },

      unary: function(fnc) {
          return function(parameter) {
              return fnc(parameter);
          };
      },

      not: function(fnc) {
          return function(item) {
              return !fnc(item);
          };
      },

      getRandomChars: function(numOfChars) {
          var start = "A".charCodeAt(0);
          var end = "z".charCodeAt(0);
          var middle = "Z".charCodeAt(0);
          var number = "0".charCodeAt(0);

          var values = crypto.getRandomValues(new window.Uint8Array(numOfChars || 1)).map(function(n) {
              var code = start + (n % (end - start));
              if (code > middle && (code - middle) < 9) {
                  code = number + code-middle;
              }
              return code;
          });

          return String.fromCharCode.apply(null, values);
      },

      getEmptyGuid: function() {
          return "00000000-0000-0000-0000-000000000000";
      },

      createGuid: function() {
          return Utils.getEmptyGuid().replace(/0/g, function(c) {
              return (c ^ crypto.getRandomValues(new window.Uint8Array(1))[0] & 15 >> c / 4).toString(16);
          });
      },

      notEmpty: function(sKey) {
          return function(item) {
              return !!Utils.result(sKey)(item);
          };
      },

      delay: function(ms) {
          return function(oData) {
              return new Promise(function (resolve, reject) {
                  setTimeout(resolve.bind(null, oData), ms); // eslint-disable-line sap-timeout-usage
              });
          };
      },

      deepCopy: function(oObject, replacer) {
          return recursiveCopy("", oObject);

          function recursiveCopy(key, value) {
              var transformed, clone;
              if (Utils.typeof(replacer) === "function") {
                  transformed = replacer(key, value);
              } else {
                  transformed = value;
              }
              if (Utils.isPrimitive(transformed)) {
                  clone = transformed;
              } else if (Utils.typeof(transformed) === "date") {
                  clone = new Date(transformed.getTime());
              } else if (Utils.typeof(transformed) === "array") {
                  clone = transformed.map(copyArrayObj);
              } else if (Utils.typeof(transformed) === "object") {
                  var names = Object.getOwnPropertyNames(transformed);
                  var descriptors = names.map(function(name) {
                      var transformedValue = transformed[name];
                      var finalValue = recursiveCopy(name, transformedValue);
                      if (finalValue === undefined) {
                          return undefined;
                      }
                      return [name, finalValue];
                  }).filter(Boolean);

                  clone = Object.fromEntries(descriptors);
              }
              return clone;
          }

          function copyArrayObj(value, index) {
              return recursiveCopy(index, value);
          }
      },

      /* Removes '__deferred' and flattens 'results' attributes */
      cleanupOData: function(oData, fnCustomMapping) {
          return Utils.deepCopy(oData, function(key, value) {
              var newValue = value;
              if (typeof fnCustomMapping === "function") {
                  newValue = fnCustomMapping(key, value);
              }
              if (key === "__metadata" || (newValue && newValue.__deferred)) {
                  newValue = undefined;
              } else if (!Utils.isPrimitive(newValue) && newValue.results) {
                  newValue = newValue.results;
              }
              return newValue;
          });
      },

      compose: function(/* some function inputs */) {
          var fns = Array.from(arguments);
          return function(x) {
              return fns.reduceRight(function(result, fn) {
                  return fn(result);
              }, x);
          };
      },

      console: function (method) {
          var args = Array.prototype.slice.call(arguments, 1);
          return function(item) {
              if (library.verbose) {
                  console[method].apply(console, args.concat(Array.from(arguments))); // eslint-disable-line no-console
              }
              return item;
          };
      },

      debounce: function(func, wait, immediate) {
          var timeout;
          return function() {
              var that = this, args = arguments;
              var later = function() {
                  timeout = null;
                  if (!immediate) {
                      func.apply(that, args);
                  }
              };
              var callNow = immediate && !timeout;
              clearTimeout(timeout);
              // eslint-disable-next-line sap-timeout-usage
              timeout = setTimeout(later, wait);
              if (callNow) {
                  func.apply(that, args);
              }
          };
      },

      error: function (logTitle) {
          return Utils.console("error", logTitle);
      },

      /**
       * returns a function which gets the propery of given object
       **/
      get: function(path, defaultValue) {
          return function(object) {
              var value = object[path];
              if (value === undefined) {
                  value = defaultValue;
              }
              if (typeof value === "function") {
                  value = value.bind(object);
              }
              return value;
          };
      },

      identity: function() {
          return function(oItem) {
              return oItem;
          };
      },

      result: function(path, defaultValue) {
          return function(object) {
              var value = Utils.get(path, defaultValue)(object);
              if (typeof value === "function") {
                  value = value.call(object);
              }
              return value;
          };
      },

      /**
       * Generates getter function for an object
       */
      getOf: function(oObject) {
          return function(path) {
              return oObject[path];
          };
      },

      log: function (logTitle) {
          return Utils.console("log", logTitle);
      },

      tap: function (sideEffectFunction) {
          return function(item) {
              sideEffectFunction(item);
              return item;
          };
      },

      mustache: function(sString, replacer) {
          //eslint-disable-next-line
          return sString.replace(new RegExp("{{[\\/\\w]+}}", "g"), function preReplacer(sMatch) {
              return replacer(sMatch.substr(2, sMatch.length - 4));
          });
      },

      mapTo: function (value) {
          return function() {
              return value;
          };
      },

      merger: function() {
          return function(obj1, obj2) {
              return Object.assign({}, obj1, obj2);
          };
      },

      nextControl: siblingsFunction("nextElementSibling"),
      previousControl: siblingsFunction("previousElementSibling"),

      str: function() {
          return Array.from(arguments).join(" ");
      },

      /**
       * Set timeout for a promise
       */
      timeout: function(ms, oPromise, sDescription){
          var promiseTimeout = new Promise(function (resolve, reject){
              // eslint-disable-next-line sap-timeout-usage
              var handle = setTimeout(function (){
                  reject(new Error(sDescription || library.getText("timeoutMessage", [ms])));
              }, ms);
              if (oPromise && oPromise.then) {
                  oPromise.then(clearInterval.bind(null, handle));
              } else {
                  clearInterval(handle);
              }
          });

          return Promise.race([oPromise, promiseTimeout]);
      },

      createMultiSelectFilter: function(fieldname, filtertype, value, separator) {
          var values = value.split(separator).map(Utils.result("trim")).filter(Boolean);
          if (values.length === 0) {
              return null;
          } else if (values.length === 1) {
              return new Filter(fieldname, filtertype, values[0]);
          }
          return new Filter({
              filters: values.map(function(item){
                  return new Filter(fieldname, filtertype, item);
              }),
              and: false
          });
      },

      /**
       * @param {array} filterList List of object to create filters like
       * @returns {array}
       *
       * [{
       *   fieldname: "Matnr",
       *   value: "ABC"
       * },{
       *   fieldname: "Product",
       *   value: DEF,
       *   filtertype: FilterOperator.EQ
       * }]
       *
       * Default filtertype is `FilterOperator.Contains`
       */
      combineFilters: function(filterList, separator, and) {
          var combined = filterList.map(function(f) {
              var value = f.value && f.value.trim();

              if (!value) {
                  return null;
              } else {
                  return Utils.createMultiSelectFilter(f.fieldname, f.filtertype || FilterOperator.Contains, value, separator);
              }
          }).filter(Boolean);

          if (combined.length > 0 && and === false) {
              combined = new Filter({
                  filters: combined,
                  and: false,
              });
          }
          return combined;
      },

      createPropertyDefinition: function(oFields) {
          return oFields.reduce(function convertToProperty(properties, field) {
              properties[field.fieldname] = {
                  type: field.type || "string",
                  defaultValue: field.defaultValue || "",
                  group: field.group || "Data",
              };
              return properties;
          }, {});
      },

      stringifyFilter: function(filterList) {
          var aConverted = filterList.map(function (v) {
              var filtertype = v.filtertype;
              var filtervalue = v.value.trim();

              if (!filtertype || filtertype === FilterOperator.Contains) {
                  filtertype = "CP";
              }

              if (filtertype === "CP") {
                  filtervalue = "*" + filtervalue + "*";
              }

              return {
                  name: v.Fieldname || v.fieldname,
                  params: [{
                      sign: "I",
                      option: filtertype,
                      low: filtervalue,
                  }],
              };
          });

          var aMerged = aConverted.reduce(function mergeByName(acc, curr) {
              var found = acc.find(Utils.byKey("name", curr.name));
              if (!found) {
                  found = {
                      name: curr.name,
                      params: [],
                  };
                  acc.push(found);
              }
              found.params = found.params.concat(curr.params);
              return acc;
          }, []);

          return JSON.stringify(aMerged);
      },

      /**
       * Sorts sap.ui.table using sortProperty values
       *
       * Utils.sortTable(oTable, [{
       *     property: "Matnr",
       *     order: sap.ui.table.SortOrder.Ascending,
       * }]);
       */
      sortTable: function(oTable, sortBy, bAddSort) {
          var addSort = !!bAddSort;
          if (oTable && sortBy) {
              var columns = oTable.getColumns();
              sortBy.forEach(function sortColumn(sort) {
                  var column = columns.filter(function getBySortProperty(col) {
                      return col.getSortProperty() === sort.property;
                  }).pop();
                  if (column) {
                      oTable.sort(column, sort.order, !!addSort);
                      addSort = true;
                  }
              });
          }
      },

      subProperties: function(oObject, aFieldList) {
          var aFields = [].concat(aFieldList).filter(Boolean);
          return Object.fromEntries(aFields.map(function createProperty(field) {
              return [field, oObject[field]];
          }));
      },

      getPrimitives: function(oObject) {
          var counter = 0;
          return Utils.deepCopy(oObject, function replacer(key, value) {
              var isRoot = (key === "" && counter++ === 0);
              return (isRoot || Utils.isPrimitive(value)) ? value : undefined;
          });
      },

      /**
       * Get selected context from sap.ui.table
       *
       * var selected = Utils.getSelected(oTable);
       */

      getSelectedContexts: function(oTable, bKeepSelection) {
          var aSelected = oTable.getSelectedIndices().map(function getContext(idx) {
              var context = oTable.getContextByIndex(idx);
              return context.getObject();
          });
          if (!bKeepSelection) {
              oTable.clearSelection();
          }
          return aSelected;
      },

      /**
       * You can use this function to filter a content list of UI5
       *
       * var textList = this.getContent().filter(Utils.byElementType("sap.m.Text"));
       */
      byElementType: function(sType) {
          return function(oElement) {
              return !sType || oElement.getMetadata().getClass().getMetadata().getName() === sType;
          };
      },

      searchInTableNearEventSource: function(oEvent) {
          var sQuery = oEvent.getParameter("query");
          var oParent = oEvent.getSource().getParent();
          var oParams = oEvent.getParameters();
          var oTable;
          var applyFilter;
          var refreshRequired = false;

          oTable = Utils.getParents(oParent, "sap.ui.table.Table")[0];

          if (!oTable && oParent.getContent) {
              oTable = oParent.getContent().filter(Utils.byElementType("sap.ui.table.Table"))[0];
          }

          if (!oTable && oParent.getItems) {
              oTable = oParent.getItems().filter(Utils.byElementType("sap.ui.table.Table"))[0];
          }

          if (!oTable) {
              return false;
          }

          if (oParams.clearButtonPressed) {
              applyFilter = [];
          } else if (oParams.refreshButtonPressed) {
              applyFilter = [];
              refreshRequired = true;
          } else if (oParams.query.trim()) {
              var fields = oTable.getColumns().map(Utils.result("getFilterProperty")).filter(Boolean);
              applyFilter = Utils.combineFilters(fields.map(function getFilterField(sFieldName) {
                  return {
                      fieldname: sFieldName,
                      value: sQuery,
                  };
              }), null, false);
          } else {
              applyFilter = [];
          }

          if (applyFilter) {
              oTable.getBinding("rows").filter(applyFilter, FilterType.Application);
          }

          return refreshRequired;
      },

      resetSearchParametersNearElement: function(oControl) {
          var oParent = oControl.getParent();
          var oTable = oParent.getContent().filter(Utils.byElementType("sap.ui.table.Table")).pop();
          var oSearchBox = oParent.getContent().filter(Utils.byElementType("sap.m.SearchField")).pop();

          if (oSearchBox) {
              oSearchBox.setValue("");
          }

          if (oTable) {
              oTable.getBinding("rows").filter(null, FilterType.Application);

              oTable.getColumns().forEach(function clearColumnFilters(oColumn) {
                  oTable.filter(oColumn, null);
              });
          }
      },

      getApplicationHashValue: function(sDetail) {
          var oDetails = sap.ushell.Container.getService("URLParsing").parseShellHash(location.hash);

          // return full data, if no parameter is given
          if (typeof sDetail === "undefined") {
              return oDetails;
          } else if (sDetail in oDetails === false) {
              throw new Error(Utils.str(sDetail, "does not exist"));
          }
          return oDetails[sDetail];
      },

      getContextIndex: function(oContext) {
          var sPath = oContext.getPath && oContext.getPath();
          return +sPath.split("/").pop();
      },

      getContextParentObject: function(oContext) {
          var aPath = oContext.getPath().split("/");
          aPath.pop(); // remove child
          var sParentPath = aPath.join("/");
          return oContext.getModel().getProperty(sParentPath);
      },

      getSemanticName: function() {
          return Utils.getApplicationHashValue("semanticObject");
      },

      getParents: function(oSource, sType) {
          var aParents = [];
          var oPointer = oSource.getParent && oSource.getParent();
          while(oPointer) {
              aParents.push(oPointer);
              oPointer = oPointer.getParent();
          }
          if (!sType) {
              return aParents;
          }
          return aParents.filter(Utils.byElementType(sType));
      },

      byId: function(oSource, sId) {
          var aParentViews = Utils.getParents(oSource, "sap.ui.core.mvc.XMLView");
          aParentViews.push(sap.ui.getCore());

          return aParentViews.map(function (oControl) {
              return oControl.byId(sId);
          }).filter(Boolean).pop();
      },

      getAction: function() {
          return Utils.getApplicationHashValue("action");
      },

      parse: function(sJsonLike) {
          try {
              return JSON.parse(sJsonLike);
          } catch (e) {
              if (e instanceof SyntaxError) {
                  return sJsonLike;
              }
              throw e;
          }
      },

      typeof: function(object) {
          return Object.prototype.toString.call(object)
                      .match(/^\[object\s(.*)\]$/)[1].toLowerCase();
      },

      isPrimitive: function(val) {
          return val === null || /^[bsnu]/.test(typeof val);
      },

      /**
       * Check if 'oObj' has similar attribute names like 'oAttrs'
       */
      validateSchema: function(oObj, oAttrs) {
          var objKeys = Object.keys(oObj);
          return (
              oObj &&
              oAttrs &&
              Object.keys(oAttrs).every(function (bKey) {
                  var attrVal = oAttrs[bKey];
                  var objVal = oObj[bKey];
                  if (typeof attrVal === "function") {
                      return attrVal(objVal);
                  }
                  if (typeof attrVal === "string") {
                      return objKeys.includes(bKey) && Utils.typeof(objVal) === attrVal;
                  }
                  if (attrVal === true) {
                      return objKeys.includes(bKey);
                  }
                  if (attrVal === false) {
                      return !objKeys.includes(bKey);
                  }
                  if (Utils.isPrimitive(attrVal)) {
                      return objKeys.includes(bKey) && attrVal === objVal;
                  }
                  return Utils.validateSchema(objVal, attrVal);
              })
          );
      },

      runnerAsOwner: function(oOwnerComponent, fnc) {
          return function() {
              var args = arguments;
              return oOwnerComponent.runAsOwner(function () {
                  return fnc.apply(null, args);
              });
          };
      },

      uniqueBy: function(aList, sKey) {
          var seen = new window.Set();

          return aList.filter(function(oItem) {
              var oValue = oItem[sKey];
              if (seen.has(oValue)) {
                  return false;
              } else {
                  seen.add(oValue);
                  return true;
              }
          });
      },
  };

  // eslint-disable-next-line sap-no-global-define
  window.Utils = Utils;

  return Utils;

  function siblingsFunction(sProperty) {
      return function(oControl, sType) {
          var $element = oControl.getDomRef();
          while(($element = $element[sProperty])) {
              if (!$element.dataset || !$element.dataset.sapUi) {
                  continue;
              }
              var oCursor = sap.ui.getCore().byId($element.dataset.sapUi);
              if (!oCursor) {
                  continue;
              }
              if (!Utils.byElementType(sType)(oCursor)) {
                  continue;
              }
              // It passed controls, so we are looking for this $element
              return oCursor;
          }
          return undefined;
      };
  }

});
```
