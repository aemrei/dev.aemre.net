---
title: UI5
---
# UI5

## Launchpad Back Button integration

### Change back behavior of launchpad back

```json
"sap.ui5": {
     "services": {
        "ShellUIService": {
            "factoryName": "sap.ushell.ui5service.ShellUIService"
        }
    }
}
```

controller.js

```js
this.getOwnerComponent().getService("ShellUIService").then(function(oShellService) {
    oShellService.setBackNavigation(function() {
        //either do nothing to disable it, or add your own back nav logic
    })
});
```

It resets to default when you back to launchpad home screen. So you don't need to reset manually.

> Default implementation path is: `/sap/ushell/components/applicationIntegration/relatedServices/RelatedServices-dbg.js`

## Other snippets

Get GET params using javascript

```js
jQuery.sap.getUriParameters().get("param1");
```

UI5 Redirect Helper

```js
sap.m.URLHelper.redirect("/sap/bc/ui5_ui5/sap/" + customData.target)
```


URL Helper for email

```js
sap.m.URLHelper.normalizeEmail(
    sFirstName + "." + sLastName + "@example.com",
    oBundle.getText("mailSubject", [sFirstName]),
    oBundle.getText("mailBody"));
```


XML view in XML view

```xml
<mvc:XMLView xmlns:mvc="sap.ui.core.mvc" viewName="net.aemre.view.Deneme" id="some_id" />
```

## Useful Libraries

* `sap.ui.base.ExpressionParser`
* `sap.ui.base.BindingParser`
* `sap.m.URLHelper`
