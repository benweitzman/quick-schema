# quick-schema
Slimmed down json schema language and validator

quick-schema defines a format for describe a restricted set of JSON document structures. It is represented in JSON and has the added benefit that a JSON document can be it's own be it's own schema. 

quick-schema infers the structure of a JSON document by looking at the types of an example document. Consider the document

```json
{
  "age": 25,
  "name": "Ben"
}
```

When interpreted with quick-schema, this document describes a schema that expects a top level object with two keys, one named `age` that should have a number value and one named `name` that should have a text value. The actual values of the keys are used only to determine the type and to give an example of intended use, but do not have a specific meaning here. 

```json
[
  {
    "age": 25,
    "name": "Ben"
  },
  {
    "street": "Prospect Street",
    "number": 54,
    "city": "Cambridge"
  }
]
```

Multiple values in lists in quick-schema give the possibility for having different subvalues. Every item in the value list must match one of the of the schemas in the schema list. 

Optional values are specific by adding a `?` to the end of a key, and exact values are specified by adding a `=`.

For example, we could encode a tagged union type:

```json
[
  {
    "type=": "Person",
    "data":
      {
        "age": 25,
        "name": "Ben"
      }
  },
  {
    "type=": "Address",
    "data":
      {
        "street": "Prospect Street,
        "number": 54,
        "city": "Cambridge"
      }
  }
]
```
