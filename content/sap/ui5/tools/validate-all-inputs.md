---
title: Validate All Inputs
---
```js
(function(container) {
    var aCustomList = [
      "sap.m.CheckBox",
      "sap.m.ComboBox",
      "sap.m.FeedListItem",
      "sap.m.Input",
      "sap.m.SelectList",
      "sap.m.Table",
      "sap.m.TextArea",
      "sap.m.UploadCollection",
    ];

    sap.ui.require([
        "sap/m/MessageBox",
        "sap/m/Popover",
    ], function(MessageBox, Popover) {
        if (!container) {
            MessageBox.error("No valid container");
            return;
        }

        var allItems = container.findAggregatedObjects(true);
        var inputControls = allItems.filter(function(item) {
            return aCustomList.includes(item.getMetadata().getName());
        });

        var invalidInputs = inputControls.filter(function(control) {
            return !validateField(control);
        });

        if (invalidInputs.length === 0) {
            MessageBox.show("Successfully validated.");
        } else {
            var oDomRef = invalidInputs[0].getDomRef();

            if (oDomRef && oDomRef.scrollIntoView) {
                oDomRef.scrollIntoView();
            }

            MessageBox.warning("Check your inputs! (" + invalidInputs.length + " invalid input)");
        }

        function validateField(oField) {
            var isValid = true;
            var elementName = oField.getMetadata().getElementName();
            do {

                if (oField.getVisible() === false) {
                    isValid = true;
                } else if (oField.getEditable && oField.getEditable() === false) {
                    isValid = true;
                } else if (oField.data("validateCheck")) {
                    var fncName = oField.data("validateCheck");
                    isValid = this[fncName](oField);
                } else if (elementName === "sap.m.ComboBox") {
                    var sSelectedKey = oField.getSelectedKey();
                    if (oField.getItems().length === 0) {
                        if (oField.getValue()) {
                            isValid = false;
                            break;
                        } else {
                            break;
                        }
                    }
                    if (!sSelectedKey) {
                        isValid = false;
                        break;
                    }
                } else if (elementName === "sap.m.Table") {
                    isValid = oField.getItems().map(validateField, this).every(Boolean);
                } else if (elementName === "sap.m.ColumnListItem") {
                    isValid = oField.getCells().map(validateField, this).every(Boolean);
                } else if (elementName === "sap.m.Input") {
                    isValid = oField.getValue() !== "";
                } else if (elementName === "sap.m.TextArea") {
                    isValid = oField.getValue() !== "";
                } else if (elementName === "sap.m.UploadCollection") {
                    isValid = oField.getItems().length > 0;
                } else if (elementName === "sap.m.Select") {
                    var sSelected = oField.getSelectedKey();
                    if (oField.getItems().length === 0) {
                        if (oField.getValue()) {
                            isValid = false;
                            break;
                        } else {
                            break;
                        }
                    }
                    if (!sSelected) {
                        isValid = false;
                        break;
                    }
                }
            } while (false);

            if (!oField.setValueState) {
                oField._valueStatePopover = new Popover({placement: "VerticalPreferredTop"});
                oField.setValueStateText = oField._valueStatePopover.setTitle.bind(oField._valueStatePopover);
                oField.setValueState = function(state) {
                    if (state !== "None") {
                        oField._valueStatePopover.openBy(oField);
                    } else {
                        oField._valueStatePopover.close();
                    }
                }
            }
            if (isValid) {
                oField.setValueState("None");
                oField.setValueStateText("");
            } else {
                oField.setValueState("Error");
                oField.setValueStateText("Invalid inpupt detacted with validateAll");
            }

            return isValid;
        }

        function dummyValidator() {
            return true;
        }

    });


})(c.byId("page"));

```
