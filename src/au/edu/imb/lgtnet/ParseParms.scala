package au.edu.imb.lgtnet

/*
 * ParseParms.scala
 *
 * ParseParms is an implementation of a command-line argument parser in Scala.
 *
 * It allows you to define:
 *
 *   A Help String
 *   Parameter entries each including:
 *       name
 *       default value
 *       regular expression used for validation; defaults are used if not stated
 *       tag to indicate a required parameter; defaults to not-required
 *   A validate method to test passed arguments against defined parameters
 *
 *   A validate method parses the arguments and returns a Scala tuple object
 *   of the form (Boolean, String, Map)
 *
 *   The Boolean indicates success and Map contains merged values: e.g.
 *   supplied command-line arguments compared and merged against defined parms.
 *   
 *   Failure, false, includes an error message in String; Map is empty
 *
 *   The String includes an error message indicating missing required parms
 *   and/or incorrect values: failed regular expression test
 *
 *   The Map object contains the merged arguments and parameter default values
 *
 *   jf.zarama at gmail dot com
 *
 *   2009.07.24
 */

class ParseParms(val usage: String) {

    private var parms = Map[String,(String,String,Boolean)]()
    private var cache: Option[String] = None    // save parm name across calls
                                                // used by req and rex methods
    def parm(name: String) = {
        parms += name -> ("", "^.*$", false ) ;cache = Some(name)
        this
    }

    def parm(name: String, default: String) = {
        parms += name -> (default, defRex(default), false); cache = Some(name)
        this
    }

    def parm(name: String, default: String, rex: String) = {
        parms += name -> (default, rex, false); cache = Some(name)
        this
    }

    def parm(name: String, default: String, rex: String, req: Boolean) = {
        parms += name -> (default, rex, req); cache = Some(name)
        this
    }

    def parm(name: String, default: String, req: Boolean) = {
        parms += name -> (default, defRex(default), req); cache =  Some(name)
        this
    }

    def req(value: Boolean) = {                 // update required flag
        val k = checkName                       // for current parameter name
        if( k.length > 0 ) {                    // stored in cache
            val pvalue = parms(k)               // parmeter tuple value
            val ntuple = (pvalue._1,pvalue._2,value)    // new tuple
            parms += cache.get -> ntuple        // update entry in parms
        }                                       // .parm("-p1","1").req(true)
        this                                    // enables chained calls
    }

    def rex(value: String) = {                  // update regular-expression
        val k = checkName                       // for current name
        if( k.length > 0 ) {                    // stored in cache
            val pvalue = parms(k)               // parameter tuple value
            val ntuple = (pvalue._1,value,pvalue._3)    // new tuple
            parms += cache.get -> ntuple        // update tuple for key in parms
        }                                       // .parm("-p1","1").rex(".+")
        this                                    // enables chained calls
    }

    private def checkName = {                           // checks name stored in cache
        cache match {                           // to be a parm-name used for
            case Some(key) => key               // req and rex methods
            case _         => ""                // req & rex will not update
        }                                       // entries if cache other than
    }                                           // Some(key)

    private def defRex(default: String): String = {
        if( default.matches("^\\d+$") ) "^\\d+$" else "^.*$"
    }

    private def genMap(args: List[String] ) = { // return a Map of args
        var argsMap = Map[String,String]()      // result object
        if( ( args.length % 2 ) != 0 ) argsMap  // must have pairs: -name value
        else {                                  // to return a valid Map
            for( i <- 0.until(args.length,2) ){ // iterate through args by 2
                argsMap += args(i) -> args(i+1) // add -name value pair
            }
            argsMap                             // return -name value Map
        }
    }

    private def testRequired( args: Map[String,String] ) = {
        val ParmsNotSupplied = new collection.mutable.ListBuffer[String]
        for{ (key,value) <- parms               // iterate trough parms
            if value._3                         // if parm is required
            if !args.contains(key)              // and it is not in args
        } ParmsNotSupplied += key               // add it to List
        ParmsNotSupplied.toList                 // empty: all required present
    }

    private def validParms( args: Map[String,String] ) = {
        val invalidParms = new collection.mutable.ListBuffer[String]
        for{ (key,value) <- args                // iterate through args
            if parms.contains(key)              // if it is a defined parm
            rex = parms(key)._2                 // parm defined rex
            if !value.matches(rex)              // if regex does not match
        } invalidParms += key                   // add invalid arg
        invalidParms.toList                     // empty: all parms valid
    }

    private def mergeParms( args: Map[String,String] ) = {
        var mergedMap = Map[String,String]()    // name value Map of results
        for{ (key,value) <- parms               // iterate through parms
            mValue = args.getOrElse(key,value._1)  // args(key) or default
        }   mergedMap +=  key -> mValue         // update result Map
        mergedMap                               // return mergedMap
    }

    private def mkString(l1: List[String],l2: List[String]) = {
        usage +
        "\n\trequired parms missing: "  +
        ( if( !l1.isEmpty ) l1.mkString(" ")  else "" )       +
        ( if( !l2.isEmpty ) "\n\tinvalid parms:          "    +
               l2.mkString(" ") + "\n" else "" )
    }

    def validate( args: List[String] ) = {          // validate args to parms
        val argsMap   = genMap( args )              // Map of args: -name value
        val reqList   = testRequired( argsMap )     // List of missing required
        val validList = validParms( argsMap )       // List of (in)valid args
        if( reqList.isEmpty && validList.isEmpty ) {// successful return
            (true,"",mergeParms( argsMap ))         // true, "", mergedParms
        } else (false,mkString(reqList,validList),Map[String,String]())
    }
}



object ParseParmsExample {

  def main(args: Array[String]) = {
    val helpString = " -p1 out.txt -p2 22 [ -p3 100 -p4 1200 ] "
    val pp = new ParseParms( helpString )
    pp.parm("-p1", "output.txt").rex("^.*\\.txt$").req(true)    // required
      .parm("-p2", "22","^\\d{2}$",true)        // alternate form, required
      .parm("-p3","100").rex("^\\d{3}$")                        // optional
      .parm("-p4","1200").rex("^\\d{4}$").req(false)            // optional

    val result = pp.validate( args.toList )
    println(  if( result._1 ) result._3  else result._2 )
    // result is a tuple (Boolean, String, Map)
    // ._1 Boolean; false: error String contained in ._2, Map in ._3 is empty
    //              true:  successful, Map of parsed & merged parms in ._3

    System.exit(0)
  }

}