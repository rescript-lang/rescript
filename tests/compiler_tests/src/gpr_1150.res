let f = children =>
  switch children {
  | list{} => []
  | list{a0, ...children} =>
    switch children {
    | list{} => [a0]
    | list{a1, ...children} =>
      switch children {
      | list{} => [a0, a1]
      | list{a2, ...children} =>
        switch children {
        | list{} => [a0, a1, a2]
        | list{a3, ...children} =>
          switch children {
          | list{} => [a0, a1, a2, a3]
          | list{a4, ...children} =>
            switch children {
            | list{} => [a0, a1, a2, a3, a4]
            | list{a5, ...children} =>
              switch children {
              | list{} => [a0, a1, a2, a3, a4, a5]
              | list{a6, ...children} =>
                switch children {
                | list{} => [a0, a1, a2, a3, a4, a5, a6]
                | list{a7, ...children} =>
                  switch children {
                  | list{} => [a0, a1, a2, a3, a4, a5, a6, a7]
                  | list{a8, ...children} =>
                    switch children {
                    | list{} => [a0, a1, a2, a3, a4, a5, a6, a7, a8]
                    | list{a9, ...children} =>
                      switch children {
                      | list{} => [a0, a1, a2, a3, a4, a5, a6, a7, a8, a9]
                      | list{a10, ...children} =>
                        switch children {
                        | list{} => [a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10]
                        | list{a11, ...children} =>
                          switch children {
                          | list{} => [a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11]
                          | list{a12, ...children} =>
                            switch children {
                            | list{} => [a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12]
                            | list{a13, ...children} =>
                              switch children {
                              | list{} => [
                                  a0,
                                  a1,
                                  a2,
                                  a3,
                                  a4,
                                  a5,
                                  a6,
                                  a7,
                                  a8,
                                  a9,
                                  a10,
                                  a11,
                                  a12,
                                  a13,
                                ]
                              | list{a14, ...children} =>
                                switch children {
                                | list{} => [
                                    a0,
                                    a1,
                                    a2,
                                    a3,
                                    a4,
                                    a5,
                                    a6,
                                    a7,
                                    a8,
                                    a9,
                                    a10,
                                    a11,
                                    a12,
                                    a13,
                                    a14,
                                  ]
                                | list{a15, ...children} =>
                                  switch children {
                                  | list{} => [
                                      a0,
                                      a1,
                                      a2,
                                      a3,
                                      a4,
                                      a5,
                                      a6,
                                      a7,
                                      a8,
                                      a9,
                                      a10,
                                      a11,
                                      a12,
                                      a13,
                                      a14,
                                      a15,
                                    ]
                                  | list{a16, ...children} => assert(false)
                                  }
                                }
                              }
                            }
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
