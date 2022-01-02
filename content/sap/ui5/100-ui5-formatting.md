---
title: UI Formatting
---

# Formatting

> To change language dependent format for testing: `sap.ui.getCore().getConfiguration().setFormatLocale("en")`

> You can parse language dependent decimal numbers with `sap.ui.core.format.NumberFormat.getFloatInstance().parse("12.000");`

## Type: `sap.ui.model.type.Currency`

| Options           | Default | Description |
| ----------------- | ------- | ----------- |
| minIntegerDigits  |         |             |
| maxIntegerDigits  |         |             |
| minFractionDigits |         |             |
| maxFractionDigits |         |             |
| decimals          |         |             |
| precision         |         |             |
| groupingSeparator |         |             |
| showMeasure       |         |             |

Assuming you have such kind of data stored in your model:

```json
{
    "data1": {
      "number1": [123, "TRY"],
    },
    "data2": {
      "number2": 567, //(value should be number, not string)
      "currency2": "TRY"
    }
}
```

you can show it in your page formatted using one of two methods:

```xml
<ObjectNumber
      number="{
            path: '/data1/number1',
            type: 'sap.ui.model.type.Currency',
            formatOptions: {showMeasure: false}
      }"
/>

<ObjectNumber
      number="{
            parts:[{path:'/data2/number2'},{path:'/data2/currency2'}],
            type: 'sap.ui.model.type.Currency',
            formatOptions: {showMeasure: true}
      }"
/>
```

or using formatter:

```js
{ //as an alternative, use formatter
      formatCurr_1: function(sal, curr) {
            var oCurrFormatter = new sap.ui.model.type.Currency();
            return oCurrFormatter.formatValue([sal, curr], "string");
      },
      formatCurr_2: function(sal, curr) {
            // Shows as a symbol like '€'
            var oBrowserLocal = sap.ui.getCore().getConfiguration().getLanguage();
            var oLocal = new sap.ui.core.Locale(oBrowserLocal);
            var oLocalData = new sap.ui.core.LocaleData(oLocal);
            return sal + " " + oLocalData.getCurrencySymbol(curr)
      }
}
```

## sap.ui.model.type.Float

| Options           | Default | Description                                    |
| ----------------- | ------- | ---------------------------------------------- |
| minIntegerDigits  | 1       | minimal number of non-fraction digits          |
| maxIntegerDigits  | 99      | maximal number of non-fraction digits          |
| minFractionDigits | 0       | minimal number of fraction digits              |
| maxFractionDigits | 99      | maximal number of fraction digits              |
| groupingEnabled   | true    | enable grouping (show the grouping separators) |
| groupingSeparator | ","     | the used grouping separator                    |
| decimalSeparator  | "."     | the used decimal separator                     |

| Available Constrains |
| -------------------- |
| maximum              |
| minimum              |

Given that you have the data:

```json
{
    "number1": 123.4, // Data (number can be number or string)
    "number2": "456.7",
}
```

you can format it like this:

```xml
<ObjectNumber
      number="{
            path: '/number1',
            type: 'sap.ui.model.type.Float'
      }"
/>

<ObjectNumber
      number="{
            path: '/number2',
            type: 'sap.ui.model.type.Float',
            formatOptions: {minFractionDigits: 2}
      }"
/>
```

## Type: `sap.ui.model.odata.type.String`

| Constraints          | Description                  |
| -------------------- | ---------------------------- |
| maxLength            | expects an integer number    |
| minLength            | expects an integer number    |
| startsWith           | expects a string             |
| startsWithIgnoreCase | expects a string             |
| endsWith             | expects a string             |
| endsWithIgnoreCase   | expects a string             |
| contains             | expects a string             |
| equals               | expects a string             |
| search               | expects a regular expression |

```xml
<Text text="{path: 'doc>Itemno', type : 'sap.ui.model.odata.type.String', constraints: {
      isDigitSequence : true,
      maxLength : 10
}}"/>
```

## Type: `sap.ui.model.odata.type.Decimal`

```xml
<Input value="{
    path: 'data/number',
    type: 'sap.ui.model.odata.type.Decimal',
    constraints: {
        scale: 'variable'
    }
}"/>
```

```xml
<core:Title text="sap.ui.model.odata.type.Decimal" />
<Label text="scale: 3" labelFor="I26"/>
<Input value="{path: 'Decimal', type: 'sap.ui.model.odata.type.Decimal', constraints: {scale: 3}}" id="I26"/>
<Label text="precision: 10, scale: &quot;variable&quot;, nullable: false"  labelFor="I27"/>
<Input value="{path: 'Decimal', type: 'sap.ui.model.odata.type.Decimal', constraints: {nullable: false, precision: 10, scale: 'variable'}}" id="I27"/>
<Label text="precision:10, scale: 3" labelFor="I28"/>
<Input value="{path: 'Decimal', type: 'sap.ui.model.odata.type.Decimal', constraints: {precision: 10, scale: 3}}" id="I28"/>
<Label text="scale: default (0), nullable: false"  labelFor="I29"/>
<Input value="{path: 'Decimal', type: 'sap.ui.model.odata.type.Decimal', constraints: {nullable: false}}" id="I29"/>
<Label text="stepInput: min: 0 max: 99 scale: 0" labelFor="stepInput"/>
<StepInput id="stepInput" min="0" max="99" value="{path: 'Decimal', type: 'sap.ui.model.odata.type.Decimal', constraints: {nullable: false, scale: 0}}"/>
<Label text="scale: &quot;variable&quot;, style: &quot;short&quot;, shortDecimals: 3" labelFor="I30"/>
<Input value="{path: 'Decimal', type: 'sap.ui.model.odata.type.Decimal', constraints: {nullable: false, scale: 'variable'}, formatOptions: {style: 'short', shortDecimals: 3}}" id="I30"/>
<Label text="precision: 10, scale: 3, minimum: 100 (exclusive), maximum: 1000" labelFor="decimalInput"/>
<Input id="decimalInput" value="{path: 'Decimal', type: 'sap.ui.model.odata.type.Decimal', constraints: {precision: 10, scale: 3, minimum: '100', minimumExclusive: true, maximum: '1000'}}"/>
```

## Type: `sap.ui.model.type.Date`

```xml
<Text
    text="{
        path: 'Edatu',
        type: 'sap.ui.model.type.Date',
        formatOptions: { pattern: 'dd/MM/yyyy' }
    }"
/>

<DatePicker
    displayFormat="short"
    change="handleChange"
    class="sapUiSmallMarginBottom"
    value="{
        path: 'json>/Trh',
        type: 'sap.ui.model.type.Date',
        formatOptions: { pattern: 'dd.MM.yyyy', UTC: true }
    }"
/>
```

```js
var oDate = new Date();
var oDateFormat = sap.ui.core.format.DateFormat.getDateTimeInstance({pattern: "dd/MM/yyyy"});
oView.byId("energytitle").setText(oDateFormat.format(sDate));
```

## Type: `sap.ui.model.odata.type.Time`

```xml
<Text text="{
        path: 'Erzet', type: 'sap.ui.model.odata.type.Time',
        formatOptions: {pattern : 'HH:mm:ss'}
    }"
 />
```

## References

* https://blogs.sap.com/2013/04/28/working-with-odata-dates/
* [Boolean](https://sapui5.hana.ondemand.com/#/topic/91f2f9396f4d1014b6dd926db0e91070.html)
* [Date](https://sapui5.hana.ondemand.com/#/topic/91f2fff06f4d1014b6dd926db0e91070)
* [DateTime](https://sapui5.hana.ondemand.com/#/topic/91f3070d6f4d1014b6dd926db0e91070)
* [Float](https://sapui5.hana.ondemand.com/#/topic/91f30dbf6f4d1014b6dd926db0e91070)
* [Integer](https://sapui5.hana.ondemand.com/#/topic/91f3145e6f4d1014b6dd926db0e91070)
* [String](https://sapui5.hana.ondemand.com/#/topic/91f31c206f4d1014b6dd926db0e91070)
* [Time](https://sapui5.hana.ondemand.com/#/topic/91f322a06f4d1014b6dd926db0e91070)
* [DateTimeInterval](https://sapui5.hana.ondemand.com/#/topic/94658aa4cfbe4fdfbd0981d78f6d9b3d)
* [FormatterClasses](https://sapui5.hana.ondemand.com/#/topic/35cbd6c6694a45f7bdbbe557f0107d63)
