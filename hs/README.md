WARNING: Process below still needs to be tested with the new directory-per-script paradigm.

To add a new script:

1. Copy the `stack` shebang header from the `core.hs` file

2a. If the header is enough for your needs, you're done setting if up!

2b. If the header needs to be modified, add an appropriate component entry to the hie.yaml file to tell it to use your customized header as a cradle for your specific file. For example, if your file's name is `example.hs`:

~~~ yaml
cradle:
  stack:
    - path: "."
      component: "core.hs"
    # Your new entry
    - path: "example.hs"
      component: "example.hs"
~~~

NB. This is necessary boilerplate until this issue is resolved: https://github.com/mpickering/hie-bios/issues/217
