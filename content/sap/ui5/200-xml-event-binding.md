---
title: XML View Event Binding
---

# XML View Event Binding

```xml
<Button text="Press Me" press="some.Helper.doSomething.call($controller, 'Hello World')"/>
<Button text="Press Me" press=".doSomething(${path: 'products>unitPrice', formatter: '.formatPrice'})" />
<Button text="Press Me" press=".doSomething(10 * ${products>unitPrice})" />
<Button text="Press Me" press=".doSomething(${products>type} === 'Laptop')" />
```

## Two new model

```xml
<Select change=".doSomething(${$parameters>/selectedItem})" />
<Button text="Press Me" press=".doSomething(${$source>/text})" />
```

## Two new variable

```xml
<Button text="Press Me" press=".doSomething($event)" />
<Button text="Press Me" press=".doSomething($controller)" />
```

## References

* [Handling Events in XML Views](https://sapui5.hana.ondemand.com/#/topic/b0fb4de7364f4bcbb053a99aa645affe)
