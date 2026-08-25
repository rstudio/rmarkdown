context('utility functions')

test_that('default_geometry() decides whether to pass a default geometry variable to Pandoc', {
  expect_false(default_geometry('geometry'))
  expect_false(default_geometry('documentclass'))
  expect_false(default_geometry(c('geometry', 'bibliography')))
  expect_true(default_geometry('output', 'classoption'))

  expect_false(default_geometry('fontsize', '--variable=documentclass:book'))
  expect_false(default_geometry('fontsize', '--metadata=documentclass:book'))
  expect_false(default_geometry('fontsize', c('--variable', 'documentclass:book')))
  expect_true(default_geometry('fontsize', c('--variable', 'graphics:true')))
})

test_that('clean_tmpfiles() only removes files created by as_tmpfile() in this process (#1632)', {
  # a stray file matching the old glob pattern, e.g. left by a parallel render
  # sharing the same tempdir(); it must survive clean_tmpfiles()
  stray <- file.path(tempdir(), paste0(tmpfile_pattern, "deadbeef.html"))
  file.create(stray)
  on.exit(unlink(stray), add = TRUE)

  .globals$tmpfiles <- NULL
  f <- as_tmpfile("<div></div>")
  expect_true(file.exists(f))
  expect_true(f %in% .globals$tmpfiles)

  clean_tmpfiles()
  expect_false(file.exists(f))       # our own file is cleaned
  expect_true(file.exists(stray))    # sibling's file is left intact
  expect_null(.globals$tmpfiles)     # registry reset
})
