
custom-caqti-type
=================

`custom-caqti-type` is a syntax extension that allows to derive custom type
from record type.

Caqti is a data base interface which help to make applicatons indendent of a
particular database. The library define some types (`string`, `int`, `bool`)
and allow the user to define its own using `Caqti_type.custom`

For example, suppose you have a record that represent your table :


.. code-block:: ocaml

  type t =
    { id: Int64.t
    ; name: string option
    ; value: float
    ; control: bool
    }

You can write a converter in Caqti which automatically convert your type to and
from the database requests :

.. code-block:: ocaml

  open Caqti_type
  let (t : t Caqti_type.t) =

    let encode { id; name; value; control } =
      Ok (id, name, value, control)

    and decode (id, name, value, control) =
      Ok { id; name; value; control }

    and rep =
        tup4 int64 (option string) float bool in

    custom ~encode ~decode rep

The goal of this extension is to generate automatically for you this code.


Usage
-----

You just have to annotate your type with the deriver `[@@deriving caqti]`. The
same code can be generated with this simple example :

.. code-block:: ocaml

  type t =
    { id: int64
    ; name: string option
    ; value: float
    ; control: bool
    } [@@deriving caqti]


How does this works ?
~~~~~~~~~~~~~~~~~~~~~

The extension take each field in the record, and call a function with the same
name as the type : the field `id: int64` will be transformed into a call to
`Caqti_type.int64` and so one. Parametrized type (like `string option`) are
transformed into a call to the appropriate function : `Caqti_type.(option
string)`

This works because Caqti define function for each primitive (`float`, `int32`,
`option` …)

You are not limited to the caqti primitive and you can define your own. Keep in
mind that if you define your own type, the extension expect you to follow the
same coding rules as caqti — each OCaml type shall have the same name as the
Caqti custom type. You can write more sophisticated code as long as there a
function with same name as the type given in the record :

.. code-block:: ocaml

  type nature =
    | Text        [@value 0]
    | Number      [@value 1]
    | Float       [@value 2]
    | Boolean     [@value 3]
  [@@deriving enum]

  let nature : nature Caqti_type.t =
    let encode n = Ok (nature_to_enum n)
    and decode n = match nature_of_enum n with
      | Some nature -> Ok nature
      | None -> Error ("Invalid nature : " ^ (Int.to_string n))
    and rep = Caqti_type.int
    in
    Caqti_type.custom ~encode ~decode rep

  type t =
    { id: int64
    ; table: Table_type.type_id (* Use an external custom type *)
    ; name: string
    ; label: string
    ; nature: nature (* Use the custom type *)
    } [@@deriving caqti]

Installation
------------

In your dune file, just add  `custom-caqti-type.ppx` in the `preprocess`
section :

.. code-block:: ocaml

 (preprocess (pps custom-caqti-type.ppx))


Licence
-------

All the code is provided under the MIT license.

(See LICENSE.txt)
