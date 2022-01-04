---
title: iframe in UI5
metaTitle: IFrame in UI5
metaDescription: You can add iframes using this snippet to a UI5 powered app
---

```xml
<mvc:View controllerName="net.aemre.demo.frame.zframe.controller.View1" xmlns:mvc="sap.ui.core.mvc" displayBlock="true" xmlns="sap.m"
        xmlns:core="sap.ui.core">
	<Shell id="shell">
		<App id="app">
			<pages>
				<Page id="page" title="{i18n>title}">
					<content>
					    <core:HTML content="&lt;iframe src='https://www.opet.com.tr/' width='100%' height='95%'/&gt;"/>
					</content>
				</Page>
			</pages>
		</App>
	</Shell>
</mvc:View>
```

#### HTML Response Headers
```
X-Frame-Options: allow-from https://example.com/
```

#### Configuring Your App
If the application is not intended to run in a frame, set `frameOptions` to `deny`:

```html
<script id="sap-ui-bootstrap"
    src="resources/sap-ui-core.js"
    data-sap-ui-frameOptions="deny">
</script>
```

##### Run with WhiteList Service
```html
<script>
window["sap-ui-config"] = {
    whitelistService: 'url/to/whitelist/service',
    frameOptions: 'trusted',
    frameOptionsConfig: {
        callback: function(bSuccess) {
            if (bSuccess) {
                alert("App is allowed to run!");
            } else {
                alert("App is not allowed to run!");
            }
        }
    }
};
</script>
<script id='sap-ui-bootstrap'
    src='resources/sap-ui-core.js'>
</script>
```
or 
```html
<meta name="sap.whitelistService" content="url/to/whitelist/service" />
<script  id='sap-ui-bootstrap'
    src='resources/sap-ui-core.js'>
</script>
```

* Whitelist service: `/sap/public/bc/uics/whitelist/service`
* Service table (transparent table): HTTP_WHITELIST
  Should be at least one entry with `ENTRY_TYPE=30`

#### References
* https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/X-Frame-Options
* https://sapui5.hana.ondemand.com/#/topic/62d9c4d8f5ad49aa914624af9551beb7
* [WhiteList Service] (https://sapui5.hana.ondemand.com/#/topic/d04a6d41480c4396af16b5d2b25509ec.html)
* https://help.sap.com/doc/erp2005_ehp_07/6.07/en-US/96/6b6233e5404ebe80513ae082131132/content.htm?no_cache=true
* Sap note: 2142551
* [Demo App](https://sapui5.hana.ondemand.com/test-resources/sap/ui/core/frameoptions/ui5parent.html)

