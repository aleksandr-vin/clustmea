{application, clustmea,
 [{description, "Cluster throughput measurer"},
  {modules, {clustmea, clustmea_sup, clustmea_conf, clustmea_task}},
  {registered, [clustmea_sup, clustmea_conf, clustmea_task]},
  {applications, [kernel, stdlib]},
  {mod, {clustmea,[]}}]}.
