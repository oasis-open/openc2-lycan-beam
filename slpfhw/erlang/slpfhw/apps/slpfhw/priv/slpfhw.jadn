{
 "meta": {
  "module": "where put",
  "title": "Https Api Globalwelcome Actuator",
  "version": "wd01-wip",
  "description": "Datatypes that define the haga commands and responses.",
  "imports": [ ["jadn", "/oasis-open.org/openc2/v1.0/jadn"] ],
  "exports": ["OpenC2-Command", "OpenC2-Response", "OpenC2-Command-Semantics"]
  },

 "types": [

  ["OpenC2-Command", "Record", [], "The OpenC2 Command describes an action performed on a target. It can be directive or descriptive depending on the context.", [
    [1, "action", "Action", [], "The task or activity to be performed (i.e., the 'verb')"],
    [2, "target", "Target", [], "The object of the action. The action is performed on the target"],
    [3, "actuator", "Actuator", ["[0"], "The subject of the action. The actuator executes the action on the target"],
    [4, "args", "Args", ["[0"], "Additional information that applies to the command"],
    [5, "id", "Command-ID", ["[0"], "Identifier used to link responses to a command"]
    ]
  ],


  ["Action", "Enumerated", [], "", [
    [1, "scan", "Systematic examination of some aspect of the entity or its environment in order to obtain information."],
    [2, "locate", "Find an object physically, logically, functionally, or by organization."],
    [3, "query", "Initiate a request for information."],
    [4, "report", "Task an entity to provide information to a designated recipient."],
    [5, "notify", "Set an entity's alerting preferences."],
    [6, "deny", "Prevent a certain event or action from completion, such as preventing a flow from reaching a destination or preventing access."],
    [7, "contain", "Isolate a file, process, or entity so that it cannot modify or access assets or processes."],
    [8, "allow", "Permit access to or execution of a target."],
    [9, "start", "Initiate a process, application, system, or activity."],
    [10, "stop", "Halt a system or end an activity."],
    [11, "restart", "Stop then start a system or activity."],
    [12, "pause", "Cease operation of a system or activity while maintaining state."],
    [13, "resume", "Start a system or activity from a paused state."],
    [14, "cancel", "Invalidate a previously issued action."],
    [15, "set", "Change a value, configuration, or state of a managed entity."],
    [16, "update", "Instruct a component to retrieve, install, process, and operate in accordance with a software update, reconfiguration, or other update."],
    [17, "move", "Change the location of a file, subnet, network, or process."],
    [18, "redirect", "Change the flow of traffic to a destination other than its original destination."],
    [19, "create", "Add a new entity of a known type (e.g., data, files, directories)."],
    [20, "delete", "Remove an entity (e.g., data, files, flows."],
    [21, "snapshot", "Record and store the state of a target at an instant in time."],
    [22, "detonate", "Execute and observe the behavior of a target (e.g., file, hyperlink) in an isolated environment."],
    [23, "restore", "Return a system to a previously known state."],
    [24, "save", "Commit data or system state to memory."],
    [25, "throttle", "Adjust the rate of a process, function, or activity."],
    [26, "delay", "Stop or hold up an activity or data transmittal."],
    [27, "substitute", "Replace all or part of the payload."],
    [28, "copy", "Duplicate a file or data flow."],
    [29, "sync", "Synchronize a sensor or actuator with other system components."],
    [30, "investigate", "Task the recipient to aggregate and report information as it pertains to a security event or incident."],
    [31, "mitigate", "Task the recipient to circumvent a problem without necessarily eliminating the vulnerability or attack point."],
    [32, "remediate", "Task the recipient to eliminate a vulnerability or attack point."]
    ]
  ],

  ["Target", "Choice", [], "OpenC2 Target datatypes", [
    [1, "artifact", "Artifact", [], "An array of bytes representing a file-like object or a link to that object."],
    [2, "command", "Command-ID", [], "A reference to a previously issued OpenC2 command"],
    [3, "device", "Device", [], "The properties of a hardware device"],
    [4, "directory", "Directory", [], "The properties common to a file system directory"],
    [5, "disk", "Disk", [], "A disk drive"],
    [6, "disk_partition", "Disk-Partition", [], "A single partition of a disk drive"],
    [7, "domain_name", "Domain-Name", [], "A netowrk domain name"],
    [8, "email_addr", "Email-Addr", [], "A single email address"],
    [9, "email_message", "Email-Message", [], "An instance of an email message, corresponding to the internet message format described in RFC 5322 and related RFCs"],
    [10, "file", "File", [], "Properties of a file"],
    [11, "ip_addr", "IP-Addr", [], "The representation of one or more IP addresses (either version 4 or version 6) expressed using CIDER notation"],
    [13, "mac_addr", "Mac-Addr", [], "A single Media Access Control (MAC) address"],
    [14, "memory", "Memory", [], "A memory object"],
    [15, "ip_connection", "IP-Connection", [], "A network connection that originates from a source and is addressed to a destination"],
    [16, "openc2", "OpenC2", [], "A set of items used with the query action to determine an actuator's capabilities"],
    [17, "process", "Process", [], "Common properties of an instance of a computer program as executed on an operating system"],
    [18, "software", "Software", [], "High-level properties associated with software, including software products"],
    [19, "uri", "URI", [], "A uniform resource identifier"],
    [20, "user_account", "User-Account", [], "An instance of any type of user account, including but not limited to operating system, device, messaging service, and social media platform accounts"],
    [21, "user_session", "User-Session", [], "A user session"],
    [22, "volume", "Volume", [], "A generic drive volume"],
    [23, "windows_registry_key", "Windows-Registry-Key", [], "The properties of a Windows registry key"],
    [24, "x509_certificate", "X509-Certificate", [], "The properties of an X.509 certificate, as defined by ITU recommendation X.509"],
    [1024, "slpf", "slpf:Target", [], "Targets defined in the Stateless Packet Filter profile"]]
  ],

  ["Actuator", "Choice", [], "", [
    [3, "x_haga", "Specifiers", [], "haga specifiers"]
    ],

  ["Specifiers", "Map", [], "", [
    [1, "whatever", "String", ["[0"], "locater of the actuator"]
    ]
  ],

  ["Args", "Map", [], "", [
    [1, "start_time", "Date-Time", ["[0"], "The specific date/time to initiate the action"],
    [2, "stop_time", "Date-Time", ["[0"], "The specific date/time to terminate the action"],
    [3, "duration", "Duration", ["[0"], "The length of time for an action to be in effect"],
    [4, "response_requested", "Response-Type", ["[0"], "The type of response required for the action"],
    [1025, "haga", "what?", ["[0"], "what?"]]
  ],

  ["OpenC2-Response", "Record", [], "", [
    [1, "id", "Command-ID", [], "Id of the ommand that induced this response"],
    [2, "status", "Status-Code", [], "An integer status code"],
    [3, "status_text", "String", ["[0"], "A free-form human-readable description of the response status"],
    [4, "*", "Results", [], "Data or extended status information that was requested from an OpenC2 command"]]
  ],

  ["Status-Code", "Enumerated", ["="], "", [
    [102, "Processing", "An interim response used to inform the client that the server has accepted the request but not yet completed it."],
    [200, "OK", "The request has succeeded."],
    [301, "Moved Permanently", "The target resource has been assigned a new permanent URI"],
    [400, "Bad Request", "The server cannot process the request due to something that is perceived to be a client error (e.g., malformed request syntax.)"],
    [401, "Unauthorized", "The request lacks valid authentication credentials for the target resources or authorization has been refused for the submitted credentials."],
    [403, "Forbidden", "The server understood the request but refuses to authorize it."],
    [500, "Server Error", "The server encountered an unexpected condition that prevented it from fulfilling the request."],
    [501, "Not Implemented", "The server does not support the functionality required to fulfill the request."]]
  ],

  ["Artifact", "Record", [], "", [
    [1, "mime_type", "String", ["[0"], "Permitted values specified in the IANA Media Types registry"],
    [2, "*", "Payload", ["[0"], "choice of literal content or URL to obtain content"],
    [3, "hashes", "Hashes", ["[0"], "Specifies a dictionary of hashes for the contents of the payload"]]
  ],

  ["Payload", "Choice", [], "", [
    [1, "payload_bin", "Binary", [], "Specifies the data contained in the artifact."],
    [2, "url", "URI", [], "MUST be a valid URL that resolves to the un-encoded content"]]
  ],

  ["OpenC2", "ArrayOf", ["*Query-Item", "[0", "]3"], "A target used to query Actuator for its supported capabilities"],

  ["Query-Item", "Enumerated", [], "Results to be included in response to query openc2 command", [
    [1, "versions", "OpenC2 language versions supported by this actuator"],
    [2, "profiles", "List of profiles supported by this actuator"],
    [3, "schema", "Definition of the command syntax supported by this actuator"]]
  ],

  ["IP-Connection", "Record", [], "5-tuple that specifies a tcp/ip connection", [
    [1, "src_addr", "IP-Addr", ["[0"], "source address"],
    [2, "src_port", "Port", ["[0"], "source TCP/UDP port number"],
    [3, "dst_addr", "IP-Addr", ["[0"], "destination address"],
    [4, "dst_port", "Port", ["[0"], "destination TCP/UDP port number"],
    [5, "layer4-protocol", "L4-Protocol", ["[0"], "Protocol (IPv4) / Next Header (IPv6)"]]
  ],

  ["L4-Protocol", "Enumerated", [], "protocol (IPv4) or next header (IPv6) field - any IANA value, RFC 5237", [
    [1, "icmp", "Internet Control Message Protocol - RFC 792"],
    [6, "tcp", "Transmission Control Protocol - RFC 793"],
    [17, "udp", "User Datagram Protocol - RFC 768"],
    [132, "sctp", "Stream Control Transmission Protocol - RFC 4960"]]
  ],

  ["File", "Map", [], "", [
    [1, "name", "String", ["[0"], "The name of the file as defined in the file system"],
    [2, "path", "String", ["[0"], "The absolute path to the location of the file in the file system"],
    [3, "hashes", "Hashes", ["[0"], "One or more cryptographic hash codes of the file contents"]]
   ],

  ["Response-Type", "Enumerated", [], "", [
    [0, "none", "No response"],
    [1, "ack", "Respond when command received"],
    [2, "complete", "Respond when all aspects of command completed"]]
  ],

  ["Process", "Map", [], "", [
    [1, "pid", "Integer", ["[0"], "Process ID of the process"],
    [2, "name", "String", ["[0"], "Name of the process"],
    [3, "cwd", "String", ["[0"], "Current working directory of the process"],
    [4, "executable", "File", ["[0"], "Executable that was executed to start the process"],
    [5, "parent", "Process", ["[0"], "Process that spawned this one"],
    [6, "command_line", "String", ["[0"], "The full command line invocation used to start this process, including all arguments"]]
  ],

  ["Hashes", "Map", [], "Cryptographic Hash values", [
    [1, "md5", "Binary", ["[0"], "Hex-encoded MD5 hash as defined in RFC3121"],
    [4, "sha1", "Binary", ["[0"], "Hex-encoded SHA1 hash as defined in RFC3174"],
    [6, "sha256", "Binary", ["[0"], "Hex-encoded SHA256 as defined in RFC6234"]]
  ],

  ["Device", "Map", [], "", [
    [1, "hostname", "Hostname", [], "A hostname that can be used to connect to this device over a network"],
    [2, "description", "String", ["[0"], "A human-readable description of the purpose, relevance, and/or properties of the device"],
    [3, "device_id", "String", ["[0"], "An identifier that refers to this device within an inventory or management system"]]
  ],

  ["Command-ID", "String", [], "Uniquely identifies a particular command - TBD syntax"],

  ["Date-Time", "String", ["@date-time"], "RFC 3339 date-time"],

  ["Duration", "String", ["@duration"], "RFC 3339 / ISO 8601 duration"],

  ["Domain-Name", "String", ["@hostname"], "Domain name, RFC 1034, section 3.5"],

  ["Email-Addr", "String", ["@email"], "Email address, RFC 5322, section 3.4.1"],

  ["IP-Addr", "String", ["@ip"], "IPv4 or IPv6 address per RFC 2673 section 3.2, and RFC 4291 section 2.2"],

  ["Mac-Addr", "String", ["@mac"], "48 bit Media Access Code address"],

  ["Port", "String", ["@port"], "Service Name or Transport Protocol Port Number, RFC 6335"],

  ["Version", "String", [], "Version string - TBD syntax"],

  ["URI", "String", ["@uri"], "Uniform Resource Identifier"],

  ["Valid_Action_Targets", "", [], "list of valid targets for each action. Note Global Welcome is an illustrative actuator so these pairings are for coverage, not that they make sense",
     [
       ["scan", [ "memory" ] ],
       ["locate", [ "device" ] ],
       ["query", [ "openc2", "hello" ] ],
       ["report", [ "x509_certificate" ] ],
       ["notify", [ "email_addr" ] ],
       ["deny", [ "ip_addr" ] ],
       ["contain", [ "mac_addr" ] ],
       ["allow", [ "ip_connection" ] ],
       ["start", [ "artifact" ] ],
       ["stop", [ "artifact" ] ],
       ["restart", [  "process"] ],
       ["pause", [ "artifact" ] ],
       ["resume", [ "artifact" ] ],
       ["cancel", [ "command" ] ],
       ["set", [ "windows_registry_key" ] ],
       ["update", [  "software"] ],
       ["move", [  "uri"] ],
       ["redirect", [ "artifact" ] ],
       ["create", [ "volume" ] ],
       ["delete", [ "file" ] ],
       ["snapshot", [ "artifact" ] ],
       ["detonate", [ "artifact" ] ],
       ["restore", [ "disk" ] ],
       ["save", [ "directory" ] ],
       ["throttle", [ "user_session" ] ],
       ["delay", [ "artifact" ] ],
       ["substitute", [ "email_addr" ] ],
       ["copy", [ "artifact" ] ],
       ["sync", [ "disk_partition" ] ],
       ["investigate", [ "domain_name" ] ],
       ["mitigate", [ "email_message" ] ],
       ["remediate", [ "user_account"] ]
     ]
  ]
 ]
 ]
}
