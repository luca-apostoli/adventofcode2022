package Day7

import Utils.Utils.lines
import zio.Console.printLine
import zio.stream.ZSink
import zio.{Scope, ZIO, ZIOAppArgs, ZIOAppDefault}

object Main extends ZIOAppDefault {

  override def run: ZIO[Environment with ZIOAppArgs with Scope, Any, Any] = {
    def rootNode = OSTree.mkRootNode
    for {
      _ <- printLine("Welcome to your Day7!")
      _ <- lines("day7.txt")
        .map(s => {
          OSParser.parse(OSParser.os, s) match {
            case OSParser.Success(d, _) => d
          }
        })
        .debug("list")
        .run(ZSink.foldLeft((rootNode, 0, 0))((incr, s: Any) => {
          incr match {
            case (tree, last, pointer) => {
              s match {
                case Cmd.CDRoot => {
                  (tree, 0, 0)
                }
                case Cmd.LS => {
                  (tree, last, pointer)
                }
                case Cmd.CDBack => {
                  (tree, last, OSTree.findParentIndex(tree, pointer)._1)
                }
                case Command("cd", Some(c)) => {
                  (
                    tree,
                    last,
                    (OSTree
                      .lookupChildIndex(tree, OSElement(c, None), pointer)
                      ._1)
                  )

                }
                case e @ OSElement(_, _) => {
                  (
                    OSTree.addChild(tree, e, pointer, last)._1,
                    last + 1,
                    pointer
                  )
                }
              }

            }
          }

        }))
        .map{case (o, _, _) => {
          OSTree.getSizes(o, Map.empty)
            .values
           .filter(_<=100000)
            .sum
        }}
        .debug("tree")

    } yield ()
  }
}
