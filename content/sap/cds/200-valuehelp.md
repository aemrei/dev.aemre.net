---
title: ValueHelp
---

# Value Helps

## Simple

```cds
@Consumption.valueHelpDefinition: [
    {entity:{name:'I_UnitOfMeasure', element: 'UnitOfMeasure'}}
]
Meins,
```

## ValueHelp additional binding

```cds
@Consumption.valueHelpDefinition: [
    {
        entity:{ name:'I_UnitOfMeasure', element: 'UnitOfMeasure' },
        additionalBinding: [ { localElement: 'MeinsText', element: '_Text' } ]
    }
]
Meins,
```

## ValueHelp

```cds

```