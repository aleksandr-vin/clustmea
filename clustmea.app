{application, clustmea,
 [{description, "Cluster throughput measurer"},
  {modules, {clustmea, clustmea_sup, clustmea_conf, clustmea_task, clustmea_reporter}},
  {registered, [clustmea_sup, clustmea_conf, clustmea_task, clustmea_reporter]},
  {applications, [kernel, stdlib]},
  {mod, {clustmea,[]}}]}.
