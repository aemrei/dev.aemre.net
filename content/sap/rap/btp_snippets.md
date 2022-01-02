# Create table snippet

    DATA:
      travels       TYPE TABLE FOR CREATE /DMO/I_Travel_M\\travel,
      bookings_cba  TYPE TABLE FOR CREATE /DMO/I_Travel_M\\travel\_booking,
      booksuppl_cba TYPE TABLE FOR CREATE /DMO/I_Travel_M\\booking\_booksupplement.

# Create UUID / GUID in BTP

```abap
cl_system_uuid=>create_uuid_x16_static( )
```