{application, clustmea,
 [{description, "Cluster throughput measurer"},
  {modules, {clustmea, clustmea_sup, clustmea_conf}},
  {registered, [clustmea_sup, clustmea_conf]},
  {applications, [kernel, stdlib]},
  {mod, {clustmea,[]}}]}.
