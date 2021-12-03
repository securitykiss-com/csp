# csp - Golang inspired concurrency library for Tcl

The csp package for Tcl is a concurrency library based on Communicating Sequential Processes. It provides two primitives, namely coroutines and channels, which allow concurrent programming in the style of Golang.

The concepts originate in Hoare's Communicating Sequential Processes, while the syntax mimics the Golang implementation.

The CSP concurrency model may be visualized as independent processes (coroutines) sending and receiving messages to the named channels. The coroutine's control flow is coordinated at the points of sending and receiving messages, i.e., the coroutine may need to wait while trying to send or receive. Since it must work in a single-threaded interpreter, waiting is non-blocking. Instead of blocking, a waiting coroutine gives way to other coroutines.

This concurrency model may also be seen as a generalization of Unix named pipes where processes and pipes correspond to coroutines and channels.

[Documentation](https://securitykiss.com/resources/tutorials/csp_project/csp.html)

[Intro](https://securitykiss.com/resources/tutorials/csp_project/index.php)

## Example

```tcl

    package require http
    package require csp
    namespace import csp::*
 
    proc main {} {
        http::geturl http://securitykiss.com/rest/slow/now -command [-> ch1]
        http::geturl http://securitykiss.com/rest/slow/now -command [-> ch2]
        timer t1 400
        select {
            <- $ch1 {
                puts "from first request: [http::data [<- $ch1]]"
            }
            <- $ch2 {
                puts "from second request: [http::data [<- $ch2]]"
            }
            <- $t1 {
                puts "requests timed out at [<- $t1]"
            }
        }
    }
 
    go main
 
    vwait forever

```
