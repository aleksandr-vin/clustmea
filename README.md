Cluster Throughput Measurement Application
==========================================

Its main goal is to centralize user operations when measuring throughput of a cluster.

Written in Erlang/OTP.

Architecture
------------

### The application consists of:

* Configuration Server *(gen_server behavior)*
* Task Server *(gen_server behavior)*
* Reports Server *(gen_event behavior)*
* Supervisor *(supervisor behavior)*

### Roles:

* User -- someone who operates the measurements of the cluster
* Application -- the Erlang/OTP application under subject AKA ClusTMea
* Application Server -- one of: Configuration Server, Task Server, Reports Server
* Worker process -- an Erlang process that makes some actions to the Cluster Nodes and reports its progress to the Reports Server
* Cluster Node -- least divisible part of the Cluster under test

### Use-cases

#### Use-case #1

1. **User** starts the application
2. **User** creates a named *configuration* on the **Configuration Server**
3. **User** modifies the *configuration* on the **Configuration Server**
4. **User** starts a measurement *task* with the *configuration* on the **Task Server**
5. **Task Server** spawns **Worker Processes** to perform a *task*
6. **Worker processes** make actions according to the *configuration* and the *task* and reports their progress to the **Reports Server**
7. **Reports Server** collects the reports
