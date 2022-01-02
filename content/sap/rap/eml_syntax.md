# EML syntax

## Create

```abap
MODIFY ENTITIES OF ZDH_MngdDocumentHeader
      ENTITY Header
        CREATE SET FIELDS WITH VALUE #( ( %cid                   = 'doc1'
                                          CompanyCode            = '0001'
                                          PurchasingOrganization = '0001' ) )
```

```abap
MODIFY ENTITIES OF zmvp_i_header IN LOCAL MODE
  ENTITY header
    UPDATE FIELDS ( Netvalue ) WITH CORRESPONDING #( lt_service_orders )
    CREATE BY \_BillingPlan
      SET FIELDS WITH billing_cba
```