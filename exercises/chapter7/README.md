Unless I'm very mistaken, this was a good use case of `traverse`. Say we want to make an _Address_ field optional, we wrap it in a `Maybe`, and then we just apply `traverse` to the validator. Eazy peazy.

```
> import Data.AddressBook
> import Data.AddressBook.Validation
> validatePerson examplePerson
pure (
  Person {
    firstName: "John",
    homeAddress: (Just Address {
      city: "FakeTown",
      state: "CA",
      street: "123 Fake St."
    }),
    lastName: "Smith",
    phones: [
      PhoneNumber {
        number: "555-555-555",
        type: HomePhone
      },
      PhoneNumber {
        number: "555-555-0000",
        type: CellPhone
      }
    ]
  }
)
```