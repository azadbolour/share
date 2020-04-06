
## List Server

### Useful Links

```
https://api.rocket.rs/v0.3/rocket_contrib/
```

### To Learn

Detailed account of String vs &str. String is a library struct. str is a
base type, a language primitive, like u32. str is always borrowed - hence &str.
It is borrowed either from a string literal or from a String.

https://blog.thoughtram.io/string-vs-str-in-rust/

https://news.ycombinator.com/item?id=16546562

### Issues

- I wanted to have a type definition for the ID of a list, and use it in
  function parameters. But to set ID to &str in a type def you need a lifetime,
  compiler suggests: pub type ID<'lifetime> = &'lifetime str; That makes the
  code sufficiently more complicated, such that the type definition is not really
  worth the effort any more. Using &str in general is problematic as in many cases it
  and the data structures that use it will require a lifetime parameter, 
  polluting the code base with lifetimes that really have nothing to do with
  application logic or the domain model.

  So even though &str is considered the preferred data structure in many
  functions that have a list ID parameter, for simpicity I am using String for
  list ID.

- Module model is unlike other languages. There is a sub-module relationship
  that is created by the use of the mod keyword. 

  What worked for me is the following. Create a file hierarchy of modules
  under src just like in other languages. The file main.rs stays in src.
  All other .rs file reside in directories under src. In each directory,
  create a dirname.mod file that includes a mod statement for each .rs
  file in that directory. Like this:

```
    pub mod list_service;
    pub mod list_service_in_memory;
    pub mod list_service_sqlite;
```

  To refer to one of the modules, you need a *path* in which super refers 
  to the parent of where you are. For example:

```
  use super::super::base::base::ListResult;
```

  The first super is like '.', the second is like '..', the first base refers to
  to the sub-directory 'base' of the parent (or equivalently to the mod.rs file
  in that sub-directory), and the second 'base' refers to the base,rs defined in
  that sub-directory.

  I am not sure the mod.rs files are strictly necessary, but for now I am going
  with what worked.

- I ran out of time trying to model the return of user errors as HTTP status
  422 (Unprocessable Entity) which seems to be the standard for user errors
  that don't fit any other HTTP status.

  Unfortunately, however, status 422 is not directly supported by the rocket web
  server framework. After struggling for hours to use that status in a simple
  and clean manner, I ran out of time and gave up.

  On second thought, however, I now believe that not using status 422 for user
  errors is a better approach anyway. Those errors are the concern of the 
  application logic and not the concern of the protocol used to communicate 
  between the client and the server. So it is easier both in rocket, and in
  general, to just return them with an Ok status, and deal with then in the 
  client application logic normally.

  That is the approach I am taking in the list server. What is returned in
  general is `Json<Result<T, ListError>>` with a status of Ok, where T is the
  return from the happy pathm and ListError is the return for errors (an enum of
  all the possible errors).

  For now, errors detected by the framework (like 404) ar not customized.

