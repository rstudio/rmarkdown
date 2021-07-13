# html_dependencies_as_string tranforms correctly

    Code
      html_dependencies_as_string(deps, ldir, odir)
    Output
      <script src="lib/bar-1.2.0/foo.js"></script>
      <script src="https://example.org/foo.js"></script>
      <script src="lib/baz-1.1.0/baz.js"></script>

