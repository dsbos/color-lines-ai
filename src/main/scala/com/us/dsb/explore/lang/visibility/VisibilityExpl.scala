package com.us.dsb.explore.lang.visibility


class VisibilityExpl {

  class C {
    def classMember(): Unit = {}

    // Companion object member:

    // - not visible by simple name (by default):
    //objectMember

    // - visible by qualified name (as usual/as elsewhere):
    C.objectMember()

    // - visible by simple name after import (as usual/as elsewhere):

    import C._
    objectMember()
  }
  object C {
    def objectMember(): Unit = {}
    (new C).classMember()
  }


}
