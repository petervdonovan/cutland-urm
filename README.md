This code is created strictly for my personal education.

Example:

```
def example_urm_program :=
  S 2;
  S 2;
  S 2;
  J 1, 2, 3;
  S 3;
  T 3, 1;
  HALT

#eval execute_big example_urm_program [2]
#eval execute_big example_urm_program [3]
```
