pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with System;
with Interfaces.C.Extensions;
with Libvirt_Common_Api; use Libvirt_Common_Api;
with Libvirt_Host_Api; use Libvirt_Host_Api;
with Interfaces.C.Strings;

package Libvirt_Domain_Api is
   pragma Preelaborate;

   VIR_DOMAIN_SCHEDULER_CPU_SHARES : aliased constant String := "cpu_shares" & ASCII.NUL;  --  /usr/include/libvirt/libvirt-domain.h:315

   VIR_DOMAIN_SCHEDULER_GLOBAL_PERIOD : aliased constant String := "global_period" & ASCII.NUL;  --  /usr/include/libvirt/libvirt-domain.h:323

   VIR_DOMAIN_SCHEDULER_GLOBAL_QUOTA : aliased constant String := "global_quota" & ASCII.NUL;  --  /usr/include/libvirt/libvirt-domain.h:331

   VIR_DOMAIN_SCHEDULER_VCPU_PERIOD : aliased constant String := "vcpu_period" & ASCII.NUL;  --  /usr/include/libvirt/libvirt-domain.h:339

   VIR_DOMAIN_SCHEDULER_VCPU_QUOTA : aliased constant String := "vcpu_quota" & ASCII.NUL;  --  /usr/include/libvirt/libvirt-domain.h:347

   VIR_DOMAIN_SCHEDULER_EMULATOR_PERIOD : aliased constant String := "emulator_period" & ASCII.NUL;  --  /usr/include/libvirt/libvirt-domain.h:356

   VIR_DOMAIN_SCHEDULER_EMULATOR_QUOTA : aliased constant String := "emulator_quota" & ASCII.NUL;  --  /usr/include/libvirt/libvirt-domain.h:365

   VIR_DOMAIN_SCHEDULER_IOTHREAD_PERIOD : aliased constant String := "iothread_period" & ASCII.NUL;  --  /usr/include/libvirt/libvirt-domain.h:373

   VIR_DOMAIN_SCHEDULER_IOTHREAD_QUOTA : aliased constant String := "iothread_quota" & ASCII.NUL;  --  /usr/include/libvirt/libvirt-domain.h:381

   VIR_DOMAIN_SCHEDULER_WEIGHT : aliased constant String := "weight" & ASCII.NUL;  --  /usr/include/libvirt/libvirt-domain.h:389

   VIR_DOMAIN_SCHEDULER_CAP : aliased constant String := "cap" & ASCII.NUL;  --  /usr/include/libvirt/libvirt-domain.h:397

   VIR_DOMAIN_SCHEDULER_RESERVATION : aliased constant String := "reservation" & ASCII.NUL;  --  /usr/include/libvirt/libvirt-domain.h:405

   VIR_DOMAIN_SCHEDULER_LIMIT : aliased constant String := "limit" & ASCII.NUL;  --  /usr/include/libvirt/libvirt-domain.h:413

   VIR_DOMAIN_SCHEDULER_SHARES : aliased constant String := "shares" & ASCII.NUL;  --  /usr/include/libvirt/libvirt-domain.h:421
   --  unsupported macro: VIR_DOMAIN_BLOCK_STATS_FIELD_LENGTH VIR_TYPED_PARAM_FIELD_LENGTH

   VIR_DOMAIN_BLOCK_STATS_READ_BYTES : aliased constant String := "rd_bytes" & ASCII.NUL;  --  /usr/include/libvirt/libvirt-domain.h:487

   VIR_DOMAIN_BLOCK_STATS_READ_REQ : aliased constant String := "rd_operations" & ASCII.NUL;  --  /usr/include/libvirt/libvirt-domain.h:495

   VIR_DOMAIN_BLOCK_STATS_READ_TOTAL_TIMES : aliased constant String := "rd_total_times" & ASCII.NUL;  --  /usr/include/libvirt/libvirt-domain.h:503

   VIR_DOMAIN_BLOCK_STATS_WRITE_BYTES : aliased constant String := "wr_bytes" & ASCII.NUL;  --  /usr/include/libvirt/libvirt-domain.h:511

   VIR_DOMAIN_BLOCK_STATS_WRITE_REQ : aliased constant String := "wr_operations" & ASCII.NUL;  --  /usr/include/libvirt/libvirt-domain.h:519

   VIR_DOMAIN_BLOCK_STATS_WRITE_TOTAL_TIMES : aliased constant String := "wr_total_times" & ASCII.NUL;  --  /usr/include/libvirt/libvirt-domain.h:527

   VIR_DOMAIN_BLOCK_STATS_FLUSH_REQ : aliased constant String := "flush_operations" & ASCII.NUL;  --  /usr/include/libvirt/libvirt-domain.h:535

   VIR_DOMAIN_BLOCK_STATS_FLUSH_TOTAL_TIMES : aliased constant String := "flush_total_times" & ASCII.NUL;  --  /usr/include/libvirt/libvirt-domain.h:543

   VIR_DOMAIN_BLOCK_STATS_ERRS : aliased constant String := "errs" & ASCII.NUL;  --  /usr/include/libvirt/libvirt-domain.h:550

   VIR_MIGRATE_PARAM_URI : aliased constant String := "migrate_uri" & ASCII.NUL;  --  /usr/include/libvirt/libvirt-domain.h:842

   VIR_MIGRATE_PARAM_DEST_NAME : aliased constant String := "destination_name" & ASCII.NUL;  --  /usr/include/libvirt/libvirt-domain.h:852

   VIR_MIGRATE_PARAM_DEST_XML : aliased constant String := "destination_xml" & ASCII.NUL;  --  /usr/include/libvirt/libvirt-domain.h:871

   VIR_MIGRATE_PARAM_PERSIST_XML : aliased constant String := "persistent_xml" & ASCII.NUL;  --  /usr/include/libvirt/libvirt-domain.h:886

   VIR_MIGRATE_PARAM_BANDWIDTH : aliased constant String := "bandwidth" & ASCII.NUL;  --  /usr/include/libvirt/libvirt-domain.h:896

   VIR_MIGRATE_PARAM_GRAPHICS_URI : aliased constant String := "graphics_uri" & ASCII.NUL;  --  /usr/include/libvirt/libvirt-domain.h:917

   VIR_MIGRATE_PARAM_LISTEN_ADDRESS : aliased constant String := "listen_address" & ASCII.NUL;  --  /usr/include/libvirt/libvirt-domain.h:928

   VIR_MIGRATE_PARAM_MIGRATE_DISKS : aliased constant String := "migrate_disks" & ASCII.NUL;  --  /usr/include/libvirt/libvirt-domain.h:937

   VIR_MIGRATE_PARAM_DISKS_PORT : aliased constant String := "disks_port" & ASCII.NUL;  --  /usr/include/libvirt/libvirt-domain.h:947

   VIR_MIGRATE_PARAM_COMPRESSION : aliased constant String := "compression" & ASCII.NUL;  --  /usr/include/libvirt/libvirt-domain.h:957

   VIR_MIGRATE_PARAM_COMPRESSION_MT_LEVEL : aliased constant String := "compression.mt.level" & ASCII.NUL;  --  /usr/include/libvirt/libvirt-domain.h:966

   VIR_MIGRATE_PARAM_COMPRESSION_MT_THREADS : aliased constant String := "compression.mt.threads" & ASCII.NUL;  --  /usr/include/libvirt/libvirt-domain.h:974

   VIR_MIGRATE_PARAM_COMPRESSION_MT_DTHREADS : aliased constant String := "compression.mt.dthreads" & ASCII.NUL;  --  /usr/include/libvirt/libvirt-domain.h:982

   VIR_MIGRATE_PARAM_COMPRESSION_XBZRLE_CACHE : aliased constant String := "compression.xbzrle.cache" & ASCII.NUL;  --  /usr/include/libvirt/libvirt-domain.h:990

   VIR_MIGRATE_PARAM_AUTO_CONVERGE_INITIAL : aliased constant String := "auto_converge.initial" & ASCII.NUL;  --  /usr/include/libvirt/libvirt-domain.h:999

   VIR_MIGRATE_PARAM_AUTO_CONVERGE_INCREMENT : aliased constant String := "auto_converge.increment" & ASCII.NUL;  --  /usr/include/libvirt/libvirt-domain.h:1009

   VIR_DOMAIN_CPU_STATS_CPUTIME : aliased constant String := "cpu_time" & ASCII.NUL;  --  /usr/include/libvirt/libvirt-domain.h:1262

   VIR_DOMAIN_CPU_STATS_USERTIME : aliased constant String := "user_time" & ASCII.NUL;  --  /usr/include/libvirt/libvirt-domain.h:1268

   VIR_DOMAIN_CPU_STATS_SYSTEMTIME : aliased constant String := "system_time" & ASCII.NUL;  --  /usr/include/libvirt/libvirt-domain.h:1274

   VIR_DOMAIN_CPU_STATS_VCPUTIME : aliased constant String := "vcpu_time" & ASCII.NUL;  --  /usr/include/libvirt/libvirt-domain.h:1281

   VIR_DOMAIN_BLKIO_WEIGHT : aliased constant String := "weight" & ASCII.NUL;  --  /usr/include/libvirt/libvirt-domain.h:1310

   VIR_DOMAIN_BLKIO_DEVICE_WEIGHT : aliased constant String := "device_weight" & ASCII.NUL;  --  /usr/include/libvirt/libvirt-domain.h:1320

   VIR_DOMAIN_BLKIO_DEVICE_READ_IOPS : aliased constant String := "device_read_iops_sec" & ASCII.NUL;  --  /usr/include/libvirt/libvirt-domain.h:1331

   VIR_DOMAIN_BLKIO_DEVICE_WRITE_IOPS : aliased constant String := "device_write_iops_sec" & ASCII.NUL;  --  /usr/include/libvirt/libvirt-domain.h:1342

   VIR_DOMAIN_BLKIO_DEVICE_READ_BPS : aliased constant String := "device_read_bytes_sec" & ASCII.NUL;  --  /usr/include/libvirt/libvirt-domain.h:1353

   VIR_DOMAIN_BLKIO_DEVICE_WRITE_BPS : aliased constant String := "device_write_bytes_sec" & ASCII.NUL;  --  /usr/include/libvirt/libvirt-domain.h:1364

   VIR_DOMAIN_MEMORY_PARAM_UNLIMITED : constant := 9007199254740991;  --  /usr/include/libvirt/libvirt-domain.h:1383

   VIR_DOMAIN_MEMORY_HARD_LIMIT : aliased constant String := "hard_limit" & ASCII.NUL;  --  /usr/include/libvirt/libvirt-domain.h:1392

   VIR_DOMAIN_MEMORY_SOFT_LIMIT : aliased constant String := "soft_limit" & ASCII.NUL;  --  /usr/include/libvirt/libvirt-domain.h:1401

   VIR_DOMAIN_MEMORY_MIN_GUARANTEE : aliased constant String := "min_guarantee" & ASCII.NUL;  --  /usr/include/libvirt/libvirt-domain.h:1410

   VIR_DOMAIN_MEMORY_SWAP_HARD_LIMIT : aliased constant String := "swap_hard_limit" & ASCII.NUL;  --  /usr/include/libvirt/libvirt-domain.h:1420

   VIR_DOMAIN_NUMA_NODESET : aliased constant String := "numa_nodeset" & ASCII.NUL;  --  /usr/include/libvirt/libvirt-domain.h:1465

   VIR_DOMAIN_NUMA_MODE : aliased constant String := "numa_mode" & ASCII.NUL;  --  /usr/include/libvirt/libvirt-domain.h:1473

   VIR_DOMAIN_BANDWIDTH_IN_AVERAGE : aliased constant String := "inbound.average" & ASCII.NUL;  --  /usr/include/libvirt/libvirt-domain.h:1585

   VIR_DOMAIN_BANDWIDTH_IN_PEAK : aliased constant String := "inbound.peak" & ASCII.NUL;  --  /usr/include/libvirt/libvirt-domain.h:1592

   VIR_DOMAIN_BANDWIDTH_IN_BURST : aliased constant String := "inbound.burst" & ASCII.NUL;  --  /usr/include/libvirt/libvirt-domain.h:1599

   VIR_DOMAIN_BANDWIDTH_IN_FLOOR : aliased constant String := "inbound.floor" & ASCII.NUL;  --  /usr/include/libvirt/libvirt-domain.h:1606

   VIR_DOMAIN_BANDWIDTH_OUT_AVERAGE : aliased constant String := "outbound.average" & ASCII.NUL;  --  /usr/include/libvirt/libvirt-domain.h:1613

   VIR_DOMAIN_BANDWIDTH_OUT_PEAK : aliased constant String := "outbound.peak" & ASCII.NUL;  --  /usr/include/libvirt/libvirt-domain.h:1620

   VIR_DOMAIN_BANDWIDTH_OUT_BURST : aliased constant String := "outbound.burst" & ASCII.NUL;  --  /usr/include/libvirt/libvirt-domain.h:1627
   --  arg-macro: function VIR_USE_CPU (cpumap, cpu)
   --    return (cpumap)((cpu) / 8) |= (2 ** ((cpu) mod 8));
   --  arg-macro: function VIR_UNUSE_CPU (cpumap, cpu)
   --    return (cpumap)((cpu) / 8) &= ~(2 ** ((cpu) mod 8));
   --  arg-macro: function VIR_CPU_USED (cpumap, cpu)
   --    return (cpumap)((cpu) / 8) and (2 ** ((cpu) mod 8));
   --  arg-macro: function VIR_CPU_MAPLEN (cpu)
   --    return ((cpu) + 7) / 8;
   --  arg-macro: procedure VIR_CPU_USABLE (cpumaps, maplen, vcpu, cpu)
   --    VIR_CPU_USED(VIR_GET_CPUMAP(cpumaps, maplen, vcpu), cpu)
   --  arg-macro: procedure VIR_COPY_CPUMAP (cpumaps, maplen, vcpu, cpumap)
   --    memcpy(cpumap, VIR_GET_CPUMAP(cpumaps, maplen, vcpu), maplen)
   --  arg-macro: function VIR_GET_CPUMAP (cpumaps, maplen, vcpu)
   --    return and((cpumaps)((vcpu) * (maplen)));

   VIR_PERF_PARAM_CMT : aliased constant String := "cmt" & ASCII.NUL;  --  /usr/include/libvirt/libvirt-domain.h:2083

   VIR_PERF_PARAM_MBMT : aliased constant String := "mbmt" & ASCII.NUL;  --  /usr/include/libvirt/libvirt-domain.h:2094

   VIR_PERF_PARAM_MBML : aliased constant String := "mbml" & ASCII.NUL;  --  /usr/include/libvirt/libvirt-domain.h:2104

   VIR_PERF_PARAM_CACHE_MISSES : aliased constant String := "cache_misses" & ASCII.NUL;  --  /usr/include/libvirt/libvirt-domain.h:2114

   VIR_PERF_PARAM_CACHE_REFERENCES : aliased constant String := "cache_references" & ASCII.NUL;  --  /usr/include/libvirt/libvirt-domain.h:2124

   VIR_PERF_PARAM_INSTRUCTIONS : aliased constant String := "instructions" & ASCII.NUL;  --  /usr/include/libvirt/libvirt-domain.h:2134

   VIR_PERF_PARAM_CPU_CYCLES : aliased constant String := "cpu_cycles" & ASCII.NUL;  --  /usr/include/libvirt/libvirt-domain.h:2144

   VIR_PERF_PARAM_BRANCH_INSTRUCTIONS : aliased constant String := "branch_instructions" & ASCII.NUL;  --  /usr/include/libvirt/libvirt-domain.h:2154

   VIR_PERF_PARAM_BRANCH_MISSES : aliased constant String := "branch_misses" & ASCII.NUL;  --  /usr/include/libvirt/libvirt-domain.h:2164

   VIR_PERF_PARAM_BUS_CYCLES : aliased constant String := "bus_cycles" & ASCII.NUL;  --  /usr/include/libvirt/libvirt-domain.h:2174

   VIR_PERF_PARAM_STALLED_CYCLES_FRONTEND : aliased constant String := "stalled_cycles_frontend" & ASCII.NUL;  --  /usr/include/libvirt/libvirt-domain.h:2185

   VIR_PERF_PARAM_STALLED_CYCLES_BACKEND : aliased constant String := "stalled_cycles_backend" & ASCII.NUL;  --  /usr/include/libvirt/libvirt-domain.h:2196

   VIR_PERF_PARAM_REF_CPU_CYCLES : aliased constant String := "ref_cpu_cycles" & ASCII.NUL;  --  /usr/include/libvirt/libvirt-domain.h:2207

   VIR_PERF_PARAM_CPU_CLOCK : aliased constant String := "cpu_clock" & ASCII.NUL;  --  /usr/include/libvirt/libvirt-domain.h:2218

   VIR_PERF_PARAM_TASK_CLOCK : aliased constant String := "task_clock" & ASCII.NUL;  --  /usr/include/libvirt/libvirt-domain.h:2229

   VIR_PERF_PARAM_PAGE_FAULTS : aliased constant String := "page_faults" & ASCII.NUL;  --  /usr/include/libvirt/libvirt-domain.h:2239

   VIR_PERF_PARAM_CONTEXT_SWITCHES : aliased constant String := "context_switches" & ASCII.NUL;  --  /usr/include/libvirt/libvirt-domain.h:2249

   VIR_PERF_PARAM_CPU_MIGRATIONS : aliased constant String := "cpu_migrations" & ASCII.NUL;  --  /usr/include/libvirt/libvirt-domain.h:2259

   VIR_PERF_PARAM_PAGE_FAULTS_MIN : aliased constant String := "page_faults_min" & ASCII.NUL;  --  /usr/include/libvirt/libvirt-domain.h:2269

   VIR_PERF_PARAM_PAGE_FAULTS_MAJ : aliased constant String := "page_faults_maj" & ASCII.NUL;  --  /usr/include/libvirt/libvirt-domain.h:2279

   VIR_PERF_PARAM_ALIGNMENT_FAULTS : aliased constant String := "alignment_faults" & ASCII.NUL;  --  /usr/include/libvirt/libvirt-domain.h:2289

   VIR_PERF_PARAM_EMULATION_FAULTS : aliased constant String := "emulation_faults" & ASCII.NUL;  --  /usr/include/libvirt/libvirt-domain.h:2299

   VIR_DOMAIN_BLOCK_COPY_BANDWIDTH : aliased constant String := "bandwidth" & ASCII.NUL;  --  /usr/include/libvirt/libvirt-domain.h:2463

   VIR_DOMAIN_BLOCK_COPY_GRANULARITY : aliased constant String := "granularity" & ASCII.NUL;  --  /usr/include/libvirt/libvirt-domain.h:2474

   VIR_DOMAIN_BLOCK_COPY_BUF_SIZE : aliased constant String := "buf-size" & ASCII.NUL;  --  /usr/include/libvirt/libvirt-domain.h:2483

   VIR_DOMAIN_BLOCK_IOTUNE_TOTAL_BYTES_SEC : aliased constant String := "total_bytes_sec" & ASCII.NUL;  --  /usr/include/libvirt/libvirt-domain.h:2524

   VIR_DOMAIN_BLOCK_IOTUNE_READ_BYTES_SEC : aliased constant String := "read_bytes_sec" & ASCII.NUL;  --  /usr/include/libvirt/libvirt-domain.h:2532

   VIR_DOMAIN_BLOCK_IOTUNE_WRITE_BYTES_SEC : aliased constant String := "write_bytes_sec" & ASCII.NUL;  --  /usr/include/libvirt/libvirt-domain.h:2540

   VIR_DOMAIN_BLOCK_IOTUNE_TOTAL_IOPS_SEC : aliased constant String := "total_iops_sec" & ASCII.NUL;  --  /usr/include/libvirt/libvirt-domain.h:2548

   VIR_DOMAIN_BLOCK_IOTUNE_READ_IOPS_SEC : aliased constant String := "read_iops_sec" & ASCII.NUL;  --  /usr/include/libvirt/libvirt-domain.h:2556

   VIR_DOMAIN_BLOCK_IOTUNE_WRITE_IOPS_SEC : aliased constant String := "write_iops_sec" & ASCII.NUL;  --  /usr/include/libvirt/libvirt-domain.h:2563

   VIR_DOMAIN_BLOCK_IOTUNE_TOTAL_BYTES_SEC_MAX : aliased constant String := "total_bytes_sec_max" & ASCII.NUL;  --  /usr/include/libvirt/libvirt-domain.h:2571

   VIR_DOMAIN_BLOCK_IOTUNE_READ_BYTES_SEC_MAX : aliased constant String := "read_bytes_sec_max" & ASCII.NUL;  --  /usr/include/libvirt/libvirt-domain.h:2579

   VIR_DOMAIN_BLOCK_IOTUNE_WRITE_BYTES_SEC_MAX : aliased constant String := "write_bytes_sec_max" & ASCII.NUL;  --  /usr/include/libvirt/libvirt-domain.h:2587

   VIR_DOMAIN_BLOCK_IOTUNE_TOTAL_IOPS_SEC_MAX : aliased constant String := "total_iops_sec_max" & ASCII.NUL;  --  /usr/include/libvirt/libvirt-domain.h:2595

   VIR_DOMAIN_BLOCK_IOTUNE_READ_IOPS_SEC_MAX : aliased constant String := "read_iops_sec_max" & ASCII.NUL;  --  /usr/include/libvirt/libvirt-domain.h:2603

   VIR_DOMAIN_BLOCK_IOTUNE_WRITE_IOPS_SEC_MAX : aliased constant String := "write_iops_sec_max" & ASCII.NUL;  --  /usr/include/libvirt/libvirt-domain.h:2610

   VIR_DOMAIN_BLOCK_IOTUNE_TOTAL_BYTES_SEC_MAX_LENGTH : aliased constant String := "total_bytes_sec_max_length" & ASCII.NUL;  --  /usr/include/libvirt/libvirt-domain.h:2618

   VIR_DOMAIN_BLOCK_IOTUNE_READ_BYTES_SEC_MAX_LENGTH : aliased constant String := "read_bytes_sec_max_length" & ASCII.NUL;  --  /usr/include/libvirt/libvirt-domain.h:2626

   VIR_DOMAIN_BLOCK_IOTUNE_WRITE_BYTES_SEC_MAX_LENGTH : aliased constant String := "write_bytes_sec_max_length" & ASCII.NUL;  --  /usr/include/libvirt/libvirt-domain.h:2634

   VIR_DOMAIN_BLOCK_IOTUNE_TOTAL_IOPS_SEC_MAX_LENGTH : aliased constant String := "total_iops_sec_max_length" & ASCII.NUL;  --  /usr/include/libvirt/libvirt-domain.h:2642

   VIR_DOMAIN_BLOCK_IOTUNE_READ_IOPS_SEC_MAX_LENGTH : aliased constant String := "read_iops_sec_max_length" & ASCII.NUL;  --  /usr/include/libvirt/libvirt-domain.h:2650

   VIR_DOMAIN_BLOCK_IOTUNE_WRITE_IOPS_SEC_MAX_LENGTH : aliased constant String := "write_iops_sec_max_length" & ASCII.NUL;  --  /usr/include/libvirt/libvirt-domain.h:2658

   VIR_DOMAIN_BLOCK_IOTUNE_SIZE_IOPS_SEC : aliased constant String := "size_iops_sec" & ASCII.NUL;  --  /usr/include/libvirt/libvirt-domain.h:2665

   VIR_DOMAIN_BLOCK_IOTUNE_GROUP_NAME : aliased constant String := "group_name" & ASCII.NUL;  --  /usr/include/libvirt/libvirt-domain.h:2672

   VIR_DOMAIN_SEND_KEY_MAX_KEYS : constant := 16;  --  /usr/include/libvirt/libvirt-domain.h:2753

   VIR_DOMAIN_JOB_OPERATION : aliased constant String := "operation" & ASCII.NUL;  --  /usr/include/libvirt/libvirt-domain.h:3165

   VIR_DOMAIN_JOB_TIME_ELAPSED : aliased constant String := "time_elapsed" & ASCII.NUL;  --  /usr/include/libvirt/libvirt-domain.h:3175

   VIR_DOMAIN_JOB_TIME_ELAPSED_NET : aliased constant String := "time_elapsed_net" & ASCII.NUL;  --  /usr/include/libvirt/libvirt-domain.h:3185

   VIR_DOMAIN_JOB_TIME_REMAINING : aliased constant String := "time_remaining" & ASCII.NUL;  --  /usr/include/libvirt/libvirt-domain.h:3195

   VIR_DOMAIN_JOB_DOWNTIME : aliased constant String := "downtime" & ASCII.NUL;  --  /usr/include/libvirt/libvirt-domain.h:3205

   VIR_DOMAIN_JOB_DOWNTIME_NET : aliased constant String := "downtime_net" & ASCII.NUL;  --  /usr/include/libvirt/libvirt-domain.h:3214

   VIR_DOMAIN_JOB_SETUP_TIME : aliased constant String := "setup_time" & ASCII.NUL;  --  /usr/include/libvirt/libvirt-domain.h:3223

   VIR_DOMAIN_JOB_DATA_TOTAL : aliased constant String := "data_total" & ASCII.NUL;  --  /usr/include/libvirt/libvirt-domain.h:3238

   VIR_DOMAIN_JOB_DATA_PROCESSED : aliased constant String := "data_processed" & ASCII.NUL;  --  /usr/include/libvirt/libvirt-domain.h:3248

   VIR_DOMAIN_JOB_DATA_REMAINING : aliased constant String := "data_remaining" & ASCII.NUL;  --  /usr/include/libvirt/libvirt-domain.h:3258

   VIR_DOMAIN_JOB_MEMORY_TOTAL : aliased constant String := "memory_total" & ASCII.NUL;  --  /usr/include/libvirt/libvirt-domain.h:3268

   VIR_DOMAIN_JOB_MEMORY_PROCESSED : aliased constant String := "memory_processed" & ASCII.NUL;  --  /usr/include/libvirt/libvirt-domain.h:3278

   VIR_DOMAIN_JOB_MEMORY_REMAINING : aliased constant String := "memory_remaining" & ASCII.NUL;  --  /usr/include/libvirt/libvirt-domain.h:3288

   VIR_DOMAIN_JOB_MEMORY_CONSTANT : aliased constant String := "memory_constant" & ASCII.NUL;  --  /usr/include/libvirt/libvirt-domain.h:3300

   VIR_DOMAIN_JOB_MEMORY_NORMAL : aliased constant String := "memory_normal" & ASCII.NUL;  --  /usr/include/libvirt/libvirt-domain.h:3310

   VIR_DOMAIN_JOB_MEMORY_NORMAL_BYTES : aliased constant String := "memory_normal_bytes" & ASCII.NUL;  --  /usr/include/libvirt/libvirt-domain.h:3320

   VIR_DOMAIN_JOB_MEMORY_BPS : aliased constant String := "memory_bps" & ASCII.NUL;  --  /usr/include/libvirt/libvirt-domain.h:3328

   VIR_DOMAIN_JOB_MEMORY_DIRTY_RATE : aliased constant String := "memory_dirty_rate" & ASCII.NUL;  --  /usr/include/libvirt/libvirt-domain.h:3336

   VIR_DOMAIN_JOB_MEMORY_PAGE_SIZE : aliased constant String := "memory_page_size" & ASCII.NUL;  --  /usr/include/libvirt/libvirt-domain.h:3347

   VIR_DOMAIN_JOB_MEMORY_ITERATION : aliased constant String := "memory_iteration" & ASCII.NUL;  --  /usr/include/libvirt/libvirt-domain.h:3358

   VIR_DOMAIN_JOB_DISK_TOTAL : aliased constant String := "disk_total" & ASCII.NUL;  --  /usr/include/libvirt/libvirt-domain.h:3368

   VIR_DOMAIN_JOB_DISK_PROCESSED : aliased constant String := "disk_processed" & ASCII.NUL;  --  /usr/include/libvirt/libvirt-domain.h:3378

   VIR_DOMAIN_JOB_DISK_REMAINING : aliased constant String := "disk_remaining" & ASCII.NUL;  --  /usr/include/libvirt/libvirt-domain.h:3388

   VIR_DOMAIN_JOB_DISK_BPS : aliased constant String := "disk_bps" & ASCII.NUL;  --  /usr/include/libvirt/libvirt-domain.h:3396

   VIR_DOMAIN_JOB_COMPRESSION_CACHE : aliased constant String := "compression_cache" & ASCII.NUL;  --  /usr/include/libvirt/libvirt-domain.h:3405

   VIR_DOMAIN_JOB_COMPRESSION_BYTES : aliased constant String := "compression_bytes" & ASCII.NUL;  --  /usr/include/libvirt/libvirt-domain.h:3413

   VIR_DOMAIN_JOB_COMPRESSION_PAGES : aliased constant String := "compression_pages" & ASCII.NUL;  --  /usr/include/libvirt/libvirt-domain.h:3421

   VIR_DOMAIN_JOB_COMPRESSION_CACHE_MISSES : aliased constant String := "compression_cache_misses" & ASCII.NUL;  --  /usr/include/libvirt/libvirt-domain.h:3430

   VIR_DOMAIN_JOB_COMPRESSION_OVERFLOW : aliased constant String := "compression_overflow" & ASCII.NUL;  --  /usr/include/libvirt/libvirt-domain.h:3440

   VIR_DOMAIN_JOB_AUTO_CONVERGE_THROTTLE : aliased constant String := "auto_converge_throttle" & ASCII.NUL;  --  /usr/include/libvirt/libvirt-domain.h:3449

   VIR_DOMAIN_TUNABLE_CPU_VCPUPIN : aliased constant String := "cputune.vcpupin%u" & ASCII.NUL;  --  /usr/include/libvirt/libvirt-domain.h:4003

   VIR_DOMAIN_TUNABLE_CPU_EMULATORPIN : aliased constant String := "cputune.emulatorpin" & ASCII.NUL;  --  /usr/include/libvirt/libvirt-domain.h:4011

   VIR_DOMAIN_TUNABLE_CPU_IOTHREADSPIN : aliased constant String := "cputune.iothreadpin%u" & ASCII.NUL;  --  /usr/include/libvirt/libvirt-domain.h:4020

   VIR_DOMAIN_TUNABLE_CPU_CPU_SHARES : aliased constant String := "cputune.cpu_shares" & ASCII.NUL;  --  /usr/include/libvirt/libvirt-domain.h:4028

   VIR_DOMAIN_TUNABLE_CPU_GLOBAL_PERIOD : aliased constant String := "cputune.global_period" & ASCII.NUL;  --  /usr/include/libvirt/libvirt-domain.h:4036

   VIR_DOMAIN_TUNABLE_CPU_GLOBAL_QUOTA : aliased constant String := "cputune.global_quota" & ASCII.NUL;  --  /usr/include/libvirt/libvirt-domain.h:4044

   VIR_DOMAIN_TUNABLE_CPU_VCPU_PERIOD : aliased constant String := "cputune.vcpu_period" & ASCII.NUL;  --  /usr/include/libvirt/libvirt-domain.h:4052

   VIR_DOMAIN_TUNABLE_CPU_VCPU_QUOTA : aliased constant String := "cputune.vcpu_quota" & ASCII.NUL;  --  /usr/include/libvirt/libvirt-domain.h:4060

   VIR_DOMAIN_TUNABLE_CPU_EMULATOR_PERIOD : aliased constant String := "cputune.emulator_period" & ASCII.NUL;  --  /usr/include/libvirt/libvirt-domain.h:4069

   VIR_DOMAIN_TUNABLE_CPU_EMULATOR_QUOTA : aliased constant String := "cputune.emulator_quota" & ASCII.NUL;  --  /usr/include/libvirt/libvirt-domain.h:4078

   VIR_DOMAIN_TUNABLE_CPU_IOTHREAD_PERIOD : aliased constant String := "cputune.iothread_period" & ASCII.NUL;  --  /usr/include/libvirt/libvirt-domain.h:4086

   VIR_DOMAIN_TUNABLE_CPU_IOTHREAD_QUOTA : aliased constant String := "cputune.iothread_quota" & ASCII.NUL;  --  /usr/include/libvirt/libvirt-domain.h:4094

   VIR_DOMAIN_TUNABLE_BLKDEV_DISK : aliased constant String := "blkdeviotune.disk" & ASCII.NUL;  --  /usr/include/libvirt/libvirt-domain.h:4102

   VIR_DOMAIN_TUNABLE_BLKDEV_TOTAL_BYTES_SEC : aliased constant String := "blkdeviotune.total_bytes_sec" & ASCII.NUL;  --  /usr/include/libvirt/libvirt-domain.h:4110

   VIR_DOMAIN_TUNABLE_BLKDEV_READ_BYTES_SEC : aliased constant String := "blkdeviotune.read_bytes_sec" & ASCII.NUL;  --  /usr/include/libvirt/libvirt-domain.h:4118

   VIR_DOMAIN_TUNABLE_BLKDEV_WRITE_BYTES_SEC : aliased constant String := "blkdeviotune.write_bytes_sec" & ASCII.NUL;  --  /usr/include/libvirt/libvirt-domain.h:4126

   VIR_DOMAIN_TUNABLE_BLKDEV_TOTAL_IOPS_SEC : aliased constant String := "blkdeviotune.total_iops_sec" & ASCII.NUL;  --  /usr/include/libvirt/libvirt-domain.h:4134

   VIR_DOMAIN_TUNABLE_BLKDEV_READ_IOPS_SEC : aliased constant String := "blkdeviotune.read_iops_sec" & ASCII.NUL;  --  /usr/include/libvirt/libvirt-domain.h:4142

   VIR_DOMAIN_TUNABLE_BLKDEV_WRITE_IOPS_SEC : aliased constant String := "blkdeviotune.write_iops_sec" & ASCII.NUL;  --  /usr/include/libvirt/libvirt-domain.h:4150

   VIR_DOMAIN_TUNABLE_BLKDEV_TOTAL_BYTES_SEC_MAX : aliased constant String := "blkdeviotune.total_bytes_sec_max" & ASCII.NUL;  --  /usr/include/libvirt/libvirt-domain.h:4158

   VIR_DOMAIN_TUNABLE_BLKDEV_READ_BYTES_SEC_MAX : aliased constant String := "blkdeviotune.read_bytes_sec_max" & ASCII.NUL;  --  /usr/include/libvirt/libvirt-domain.h:4166

   VIR_DOMAIN_TUNABLE_BLKDEV_WRITE_BYTES_SEC_MAX : aliased constant String := "blkdeviotune.write_bytes_sec_max" & ASCII.NUL;  --  /usr/include/libvirt/libvirt-domain.h:4174

   VIR_DOMAIN_TUNABLE_BLKDEV_TOTAL_IOPS_SEC_MAX : aliased constant String := "blkdeviotune.total_iops_sec_max" & ASCII.NUL;  --  /usr/include/libvirt/libvirt-domain.h:4182

   VIR_DOMAIN_TUNABLE_BLKDEV_READ_IOPS_SEC_MAX : aliased constant String := "blkdeviotune.read_iops_sec_max" & ASCII.NUL;  --  /usr/include/libvirt/libvirt-domain.h:4190

   VIR_DOMAIN_TUNABLE_BLKDEV_WRITE_IOPS_SEC_MAX : aliased constant String := "blkdeviotune.write_iops_sec_max" & ASCII.NUL;  --  /usr/include/libvirt/libvirt-domain.h:4198

   VIR_DOMAIN_TUNABLE_BLKDEV_SIZE_IOPS_SEC : aliased constant String := "blkdeviotune.size_iops_sec" & ASCII.NUL;  --  /usr/include/libvirt/libvirt-domain.h:4206

   VIR_DOMAIN_TUNABLE_BLKDEV_GROUP_NAME : aliased constant String := "blkdeviotune.group_name" & ASCII.NUL;  --  /usr/include/libvirt/libvirt-domain.h:4214

   VIR_DOMAIN_TUNABLE_BLKDEV_TOTAL_BYTES_SEC_MAX_LENGTH : aliased constant String := "blkdeviotune.total_bytes_sec_max_length" & ASCII.NUL;  --  /usr/include/libvirt/libvirt-domain.h:4223

   VIR_DOMAIN_TUNABLE_BLKDEV_READ_BYTES_SEC_MAX_LENGTH : aliased constant String := "blkdeviotune.read_bytes_sec_max_length" & ASCII.NUL;  --  /usr/include/libvirt/libvirt-domain.h:4232

   VIR_DOMAIN_TUNABLE_BLKDEV_WRITE_BYTES_SEC_MAX_LENGTH : aliased constant String := "blkdeviotune.write_bytes_sec_max_length" & ASCII.NUL;  --  /usr/include/libvirt/libvirt-domain.h:4241

   VIR_DOMAIN_TUNABLE_BLKDEV_TOTAL_IOPS_SEC_MAX_LENGTH : aliased constant String := "blkdeviotune.total_iops_sec_max_length" & ASCII.NUL;  --  /usr/include/libvirt/libvirt-domain.h:4250

   VIR_DOMAIN_TUNABLE_BLKDEV_READ_IOPS_SEC_MAX_LENGTH : aliased constant String := "blkdeviotune.read_iops_sec_max_length" & ASCII.NUL;  --  /usr/include/libvirt/libvirt-domain.h:4259

   VIR_DOMAIN_TUNABLE_BLKDEV_WRITE_IOPS_SEC_MAX_LENGTH : aliased constant String := "blkdeviotune.write_iops_sec_max_length" & ASCII.NUL;  --  /usr/include/libvirt/libvirt-domain.h:4268
   --  arg-macro: function VIR_DOMAIN_EVENT_CALLBACK (cb)
   --    return (virConnectDomainEventGenericCallback)(cb);
   --  unsupported macro: VIR_DOMAIN_SCHED_FIELD_LENGTH VIR_TYPED_PARAM_FIELD_LENGTH
   --  unsupported macro: VIR_DOMAIN_BLKIO_FIELD_LENGTH VIR_TYPED_PARAM_FIELD_LENGTH
   --  unsupported macro: VIR_DOMAIN_MEMORY_FIELD_LENGTH VIR_TYPED_PARAM_FIELD_LENGTH

  -- * libvirt-domain.h
  -- * Summary: APIs for management of domains
  -- * Description: Provides APIs for the management of domains
  -- * Author: Daniel Veillard <veillard@redhat.com>
  -- *
  -- * Copyright (C) 2006-2015 Red Hat, Inc.
  -- *
  -- * This library is free software; you can redistribute it and/or
  -- * modify it under the terms of the GNU Lesser General Public
  -- * License as published by the Free Software Foundation; either
  -- * version 2.1 of the License, or (at your option) any later version.
  -- *
  -- * This library is distributed in the hope that it will be useful,
  -- * but WITHOUT ANY WARRANTY; without even the implied warranty of
  -- * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  -- * Lesser General Public License for more details.
  -- *
  -- * You should have received a copy of the GNU Lesser General Public
  -- * License along with this library.  If not, see
  -- * <http://www.gnu.org/licenses/>.
  --  

  --*
  -- * virDomain:
  -- *
  -- * a virDomain is a private structure representing a domain.
  --  

   --  skipped empty struct u_virDomain

   --  skipped empty struct virDomain

  --*
  -- * virDomainPtr:
  -- *
  -- * a virDomainPtr is pointer to a virDomain private structure, this is the
  -- * type used to reference a domain in the API.
  --  

   type virDomainPtr is new System.Address;  -- /usr/include/libvirt/libvirt-domain.h:45

  --*
  -- * virDomainState:
  -- *
  -- * A domain may be in different states at a given point in time
  --  

  -- no state  
  -- the domain is running  
  -- the domain is blocked on resource  
  -- the domain is paused by user  
  -- the domain is being shut down  
  -- the domain is shut off  
  -- the domain is crashed  
  -- the domain is suspended by guest
  --                                   power management  

  --     * NB: this enum value will increase over time as new events are
  --     * added to the libvirt API. It reflects the last state supported
  --     * by this version of the libvirt API.
  --      

   type virDomainState is 
     (VIR_DOMAIN_NOSTATE,
      VIR_DOMAIN_RUNNING,
      VIR_DOMAIN_BLOCKED,
      VIR_DOMAIN_PAUSED,
      VIR_DOMAIN_SHUTDOWN,
      VIR_DOMAIN_SHUTOFF,
      VIR_DOMAIN_CRASHED,
      VIR_DOMAIN_PMSUSPENDED);
   pragma Convention (C, virDomainState);  -- /usr/include/libvirt/libvirt-domain.h:71

   type virDomainNostateReason is 
     (VIR_DOMAIN_NOSTATE_UNKNOWN);
   pragma Convention (C, virDomainNostateReason);  -- /usr/include/libvirt/libvirt-domain.h:79

  -- normal startup from boot  
  -- migrated from another host  
  -- restored from a state file  
  -- restored from snapshot  
  -- returned from paused state  
  -- returned from migration  
  -- returned from failed save process  
  -- returned from pmsuspended due to
  --                                               wakeup event  

  -- resumed from crashed  
  -- running in post-copy migration mode  
   type virDomainRunningReason is 
     (VIR_DOMAIN_RUNNING_UNKNOWN,
      VIR_DOMAIN_RUNNING_BOOTED,
      VIR_DOMAIN_RUNNING_MIGRATED,
      VIR_DOMAIN_RUNNING_RESTORED,
      VIR_DOMAIN_RUNNING_FROM_SNAPSHOT,
      VIR_DOMAIN_RUNNING_UNPAUSED,
      VIR_DOMAIN_RUNNING_MIGRATION_CANCELED,
      VIR_DOMAIN_RUNNING_SAVE_CANCELED,
      VIR_DOMAIN_RUNNING_WAKEUP,
      VIR_DOMAIN_RUNNING_CRASHED,
      VIR_DOMAIN_RUNNING_POSTCOPY);
   pragma Convention (C, virDomainRunningReason);  -- /usr/include/libvirt/libvirt-domain.h:98

  -- the reason is unknown  
   type virDomainBlockedReason is 
     (VIR_DOMAIN_BLOCKED_UNKNOWN);
   pragma Convention (C, virDomainBlockedReason);  -- /usr/include/libvirt/libvirt-domain.h:106

  -- the reason is unknown  
  -- paused on user request  
  -- paused for offline migration  
  -- paused for save  
  -- paused for offline core dump  
  -- paused due to a disk I/O error  
  -- paused due to a watchdog event  
  -- paused after restoring from snapshot  
  -- paused during shutdown process  
  -- paused while creating a snapshot  
  -- paused due to a guest crash  
  -- the domain is being started  
  -- paused for post-copy migration  
  -- paused after failed post-copy  
   type virDomainPausedReason is 
     (VIR_DOMAIN_PAUSED_UNKNOWN,
      VIR_DOMAIN_PAUSED_USER,
      VIR_DOMAIN_PAUSED_MIGRATION,
      VIR_DOMAIN_PAUSED_SAVE,
      VIR_DOMAIN_PAUSED_DUMP,
      VIR_DOMAIN_PAUSED_IOERROR,
      VIR_DOMAIN_PAUSED_WATCHDOG,
      VIR_DOMAIN_PAUSED_FROM_SNAPSHOT,
      VIR_DOMAIN_PAUSED_SHUTTING_DOWN,
      VIR_DOMAIN_PAUSED_SNAPSHOT,
      VIR_DOMAIN_PAUSED_CRASHED,
      VIR_DOMAIN_PAUSED_STARTING_UP,
      VIR_DOMAIN_PAUSED_POSTCOPY,
      VIR_DOMAIN_PAUSED_POSTCOPY_FAILED);
   pragma Convention (C, virDomainPausedReason);  -- /usr/include/libvirt/libvirt-domain.h:127

  -- the reason is unknown  
  -- shutting down on user request  
   type virDomainShutdownReason is 
     (VIR_DOMAIN_SHUTDOWN_UNKNOWN,
      VIR_DOMAIN_SHUTDOWN_USER);
   pragma Convention (C, virDomainShutdownReason);  -- /usr/include/libvirt/libvirt-domain.h:136

  -- the reason is unknown  
  -- normal shutdown  
  -- forced poweroff  
  -- domain crashed  
  -- migrated to another host  
  -- saved to a file  
  -- domain failed to start  
  -- restored from a snapshot which was
  --                                           * taken while domain was shutoff  

   type virDomainShutoffReason is 
     (VIR_DOMAIN_SHUTOFF_UNKNOWN,
      VIR_DOMAIN_SHUTOFF_SHUTDOWN,
      VIR_DOMAIN_SHUTOFF_DESTROYED,
      VIR_DOMAIN_SHUTOFF_CRASHED,
      VIR_DOMAIN_SHUTOFF_MIGRATED,
      VIR_DOMAIN_SHUTOFF_SAVED,
      VIR_DOMAIN_SHUTOFF_FAILED,
      VIR_DOMAIN_SHUTOFF_FROM_SNAPSHOT);
   pragma Convention (C, virDomainShutoffReason);  -- /usr/include/libvirt/libvirt-domain.h:151

  -- crashed for unknown reason  
  -- domain panicked  
   type virDomainCrashedReason is 
     (VIR_DOMAIN_CRASHED_UNKNOWN,
      VIR_DOMAIN_CRASHED_PANICKED);
   pragma Convention (C, virDomainCrashedReason);  -- /usr/include/libvirt/libvirt-domain.h:160

   type virDomainPMSuspendedReason is 
     (VIR_DOMAIN_PMSUSPENDED_UNKNOWN);
   pragma Convention (C, virDomainPMSuspendedReason);  -- /usr/include/libvirt/libvirt-domain.h:168

   type virDomainPMSuspendedDiskReason is 
     (VIR_DOMAIN_PMSUSPENDED_DISK_UNKNOWN);
   pragma Convention (C, virDomainPMSuspendedDiskReason);  -- /usr/include/libvirt/libvirt-domain.h:176

  --*
  -- * virDomainControlState:
  -- *
  -- * Current state of a control interface to the domain.
  --  

  -- operational, ready to accept commands  
  -- background job is running (can be
  --                                        monitored by virDomainGetJobInfo); only
  --                                        limited set of commands may be allowed  

  -- occupied by a running command  
  -- unusable, domain cannot be fully
  --                                        operated, possible reason is provided
  --                                        in the details field  

   type virDomainControlState is 
     (VIR_DOMAIN_CONTROL_OK,
      VIR_DOMAIN_CONTROL_JOB,
      VIR_DOMAIN_CONTROL_OCCUPIED,
      VIR_DOMAIN_CONTROL_ERROR);
   pragma Convention (C, virDomainControlState);  -- /usr/include/libvirt/libvirt-domain.h:196

  --*
  -- * virDomainControlErrorReason:
  -- *
  -- * Reason for the error state.
  --  

  -- server didn't provide a
  --                                                     reason  

  -- unknown reason for the
  --                                                     error  

  -- monitor connection is
  --                                                     broken  

  -- error caused due to
  --                                                     internal failure in libvirt
  --                                                   

   type virDomainControlErrorReason is 
     (VIR_DOMAIN_CONTROL_ERROR_REASON_NONE,
      VIR_DOMAIN_CONTROL_ERROR_REASON_UNKNOWN,
      VIR_DOMAIN_CONTROL_ERROR_REASON_MONITOR,
      VIR_DOMAIN_CONTROL_ERROR_REASON_INTERNAL);
   pragma Convention (C, virDomainControlErrorReason);  -- /usr/include/libvirt/libvirt-domain.h:216

  --*
  -- * virDomainControlInfo:
  -- *
  -- * Structure filled in by virDomainGetControlInfo and providing details about
  -- * current state of control interface to a domain.
  --  

   type u_virDomainControlInfo;
   subtype virDomainControlInfo is u_virDomainControlInfo;

  -- control state, one of virDomainControlState  
   type u_virDomainControlInfo is record
      state : aliased unsigned;  -- /usr/include/libvirt/libvirt-domain.h:226
      details : aliased unsigned;  -- /usr/include/libvirt/libvirt-domain.h:227
      stateTime : aliased Extensions.unsigned_long_long;  -- /usr/include/libvirt/libvirt-domain.h:229
   end record;
   pragma Convention (C_Pass_By_Copy, u_virDomainControlInfo);  -- /usr/include/libvirt/libvirt-domain.h:225

  -- state details, currently 0 except for ERROR
  --                               state (one of virDomainControlErrorReason)  

  -- for how long (in msec) control interface
  --                                     has been in current state (except for OK
  --                                     and ERROR states)  

  --*
  -- * virDomainControlInfoPtr:
  -- *
  -- * Pointer to virDomainControlInfo structure.
  --  

   type virDomainControlInfoPtr is access all virDomainControlInfo;  -- /usr/include/libvirt/libvirt-domain.h:239

  --*
  -- * virDomainModificationImpact:
  -- *
  -- * Several modification APIs take flags to determine whether a change
  -- * to the domain affects just the running instance, just the
  -- * persistent definition, or both at the same time.  The counterpart
  -- * query APIs also take the same flags to determine whether to query
  -- * the running instance or persistent definition, although both cannot
  -- * be queried at once.
  -- *
  -- * The use of VIR_DOMAIN_AFFECT_CURRENT will resolve to either
  -- * VIR_DOMAIN_AFFECT_LIVE or VIR_DOMAIN_AFFECT_CONFIG according to
  -- * current domain state. VIR_DOMAIN_AFFECT_LIVE requires a running
  -- * domain, and VIR_DOMAIN_AFFECT_CONFIG requires a persistent domain
  -- * (whether or not it is running).
  -- *
  -- * These enums should not conflict with those of virTypedParameterFlags.
  --  

  -- Affect current domain state.   
  -- Affect running domain state.   
  -- Affect persistent domain state.   
  -- 1 << 2 is reserved for virTypedParameterFlags  
   type virDomainModificationImpact is 
     (VIR_DOMAIN_AFFECT_CURRENT,
      VIR_DOMAIN_AFFECT_LIVE,
      VIR_DOMAIN_AFFECT_CONFIG);
   pragma Convention (C, virDomainModificationImpact);  -- /usr/include/libvirt/libvirt-domain.h:264

  --*
  -- * virDomainInfoPtr:
  -- *
  -- * a virDomainInfo is a structure filled by virDomainGetInfo() and extracting
  -- * runtime information for a given active Domain
  --  

   type u_virDomainInfo;
   subtype virDomainInfo is u_virDomainInfo;

  -- the running state, one of virDomainState  
   type u_virDomainInfo is record
      state : aliased unsigned_char;  -- /usr/include/libvirt/libvirt-domain.h:276
      maxMem : aliased unsigned_long;  -- /usr/include/libvirt/libvirt-domain.h:277
      memory : aliased unsigned_long;  -- /usr/include/libvirt/libvirt-domain.h:278
      nrVirtCpu : aliased unsigned_short;  -- /usr/include/libvirt/libvirt-domain.h:279
      cpuTime : aliased Extensions.unsigned_long_long;  -- /usr/include/libvirt/libvirt-domain.h:280
   end record;
   pragma Convention (C_Pass_By_Copy, u_virDomainInfo);  -- /usr/include/libvirt/libvirt-domain.h:275

  -- the maximum memory in KBytes allowed  
  -- the memory in KBytes used by the domain  
  -- the number of virtual CPUs for the domain  
  -- the CPU time used in nanoseconds  
  --*
  -- * virDomainInfoPtr:
  -- *
  -- * a virDomainInfoPtr is a pointer to a virDomainInfo structure.
  --  

   type virDomainInfoPtr is access all virDomainInfo;  -- /usr/include/libvirt/libvirt-domain.h:289

  --*
  -- * virDomainCreateFlags:
  -- *
  -- * Flags OR'ed together to provide specific behaviour when creating a
  -- * Domain.
  --  

  -- Default behavior  
  -- Launch guest in paused state  
  -- Automatically kill guest when virConnectPtr is closed  
  -- Avoid file system cache pollution  
  -- Boot, discarding any managed save  
  -- Validate the XML document against schema  
   subtype virDomainCreateFlags is unsigned;
   VIR_DOMAIN_NONE : constant virDomainCreateFlags := 0;
   VIR_DOMAIN_START_PAUSED : constant virDomainCreateFlags := 1;
   VIR_DOMAIN_START_AUTODESTROY : constant virDomainCreateFlags := 2;
   VIR_DOMAIN_START_BYPASS_CACHE : constant virDomainCreateFlags := 4;
   VIR_DOMAIN_START_FORCE_BOOT : constant virDomainCreateFlags := 8;
   VIR_DOMAIN_START_VALIDATE : constant virDomainCreateFlags := 16;  -- /usr/include/libvirt/libvirt-domain.h:304

  -- Management of scheduler parameters  
  --*
  -- * VIR_DOMAIN_SCHEDULER_CPU_SHARES:
  -- *
  -- * Macro represents proportional weight of the scheduler used on the
  -- * host cpu, when using the posix scheduler, as a ullong.
  --  

  --*
  -- * VIR_DOMAIN_SCHEDULER_GLOBAL_PERIOD:
  -- *
  -- * Macro represents the enforcement period for a quota, in microseconds,
  -- * for whole domain, when using the posix scheduler, as a ullong.
  --  

  --*
  -- * VIR_DOMAIN_SCHEDULER_GLOBAL_QUOTA:
  -- *
  -- * Macro represents the maximum bandwidth to be used within a period for
  -- * whole domain, when using the posix scheduler, as an llong.
  --  

  --*
  -- * VIR_DOMAIN_SCHEDULER_VCPU_PERIOD:
  -- *
  -- * Macro represents the enforcement period for a quota, in microseconds,
  -- * for vcpus only, when using the posix scheduler, as a ullong.
  --  

  --*
  -- * VIR_DOMAIN_SCHEDULER_VCPU_QUOTA:
  -- *
  -- * Macro represents the maximum bandwidth to be used within a period for
  -- * vcpus only, when using the posix scheduler, as an llong.
  --  

  --*
  -- * VIR_DOMAIN_SCHEDULER_EMULATOR_PERIOD:
  -- *
  -- * Macro represents the enforcement period for a quota in microseconds,
  -- * when using the posix scheduler, for all emulator activity not tied to
  -- * vcpus, as a ullong.
  --  

  --*
  -- * VIR_DOMAIN_SCHEDULER_EMULATOR_QUOTA:
  -- *
  -- * Macro represents the maximum bandwidth to be used within a period for
  -- * all emulator activity not tied to vcpus, when using the posix scheduler,
  -- * as an llong.
  --  

  --*
  -- * VIR_DOMAIN_SCHEDULER_IOTHREAD_PERIOD:
  -- *
  -- * Macro represents the enforcement period for a quota, in microseconds,
  -- * for IOThreads only, when using the posix scheduler, as a ullong.
  --  

  --*
  -- * VIR_DOMAIN_SCHEDULER_IOTHREAD_QUOTA:
  -- *
  -- * Macro represents the maximum bandwidth to be used within a period for
  -- * IOThreads only, when using the posix scheduler, as an llong.
  --  

  --*
  -- * VIR_DOMAIN_SCHEDULER_WEIGHT:
  -- *
  -- * Macro represents the relative weight,  when using the credit
  -- * scheduler, as a uint.
  --  

  --*
  -- * VIR_DOMAIN_SCHEDULER_CAP:
  -- *
  -- * Macro represents the maximum scheduler cap, when using the credit
  -- * scheduler, as a uint.
  --  

  --*
  -- * VIR_DOMAIN_SCHEDULER_RESERVATION:
  -- *
  -- * Macro represents the scheduler reservation value, when using the
  -- * allocation scheduler, as an llong.
  --  

  --*
  -- * VIR_DOMAIN_SCHEDULER_LIMIT:
  -- *
  -- * Macro represents the scheduler limit value, when using the
  -- * allocation scheduler, as an llong.
  --  

  --*
  -- * VIR_DOMAIN_SCHEDULER_SHARES:
  -- *
  -- * Macro represents the scheduler shares value, when using the
  -- * allocation scheduler, as an int.
  --  

  -- * Fetch scheduler parameters, caller allocates 'params' field of size 'nparams'
  --  

   function virDomainGetSchedulerParameters
     (domain : virDomainPtr;
      params : virTypedParameterPtr;
      nparams : access int) return int;  -- /usr/include/libvirt/libvirt-domain.h:426
   pragma Import (C, virDomainGetSchedulerParameters, "virDomainGetSchedulerParameters");

   function virDomainGetSchedulerParametersFlags
     (domain : virDomainPtr;
      params : virTypedParameterPtr;
      nparams : access int;
      flags : unsigned) return int;  -- /usr/include/libvirt/libvirt-domain.h:429
   pragma Import (C, virDomainGetSchedulerParametersFlags, "virDomainGetSchedulerParametersFlags");

  -- * Change scheduler parameters
  --  

   function virDomainSetSchedulerParameters
     (domain : virDomainPtr;
      params : virTypedParameterPtr;
      nparams : int) return int;  -- /usr/include/libvirt/libvirt-domain.h:437
   pragma Import (C, virDomainSetSchedulerParameters, "virDomainSetSchedulerParameters");

   function virDomainSetSchedulerParametersFlags
     (domain : virDomainPtr;
      params : virTypedParameterPtr;
      nparams : int;
      flags : unsigned) return int;  -- /usr/include/libvirt/libvirt-domain.h:440
   pragma Import (C, virDomainSetSchedulerParametersFlags, "virDomainSetSchedulerParametersFlags");

  --*
  -- * virDomainBlockStats:
  -- *
  -- * Block device stats for virDomainBlockStats.
  -- *
  -- * Hypervisors may return a field set to ((long long)-1) which indicates
  -- * that the hypervisor does not support that statistic.
  -- *
  -- * NB. Here 'long long' means 64 bit integer.
  --  

   type u_virDomainBlockStats;
   subtype virDomainBlockStatsStruct is u_virDomainBlockStats;

  -- number of read requests  
   type u_virDomainBlockStats is record
      rd_req : aliased Long_Long_Integer;  -- /usr/include/libvirt/libvirt-domain.h:458
      rd_bytes : aliased Long_Long_Integer;  -- /usr/include/libvirt/libvirt-domain.h:459
      wr_req : aliased Long_Long_Integer;  -- /usr/include/libvirt/libvirt-domain.h:460
      wr_bytes : aliased Long_Long_Integer;  -- /usr/include/libvirt/libvirt-domain.h:461
      errs : aliased Long_Long_Integer;  -- /usr/include/libvirt/libvirt-domain.h:462
   end record;
   pragma Convention (C_Pass_By_Copy, u_virDomainBlockStats);  -- /usr/include/libvirt/libvirt-domain.h:457

  -- number of read bytes  
  -- number of write requests  
  -- number of written bytes  
  -- In Xen this returns the mysterious 'oo_req'.  
  --*
  -- * virDomainBlockStatsPtr:
  -- *
  -- * A pointer to a virDomainBlockStats structure
  --  

   type virDomainBlockStatsPtr is access all virDomainBlockStatsStruct;  -- /usr/include/libvirt/libvirt-domain.h:470

  --*
  -- * VIR_DOMAIN_BLOCK_STATS_FIELD_LENGTH:
  -- *
  -- * Macro providing the field length of parameter names when using
  -- * virDomainBlockStatsFlags().
  --  

  --*
  -- * VIR_DOMAIN_BLOCK_STATS_READ_BYTES:
  -- *
  -- * Macro represents the total number of read bytes of the
  -- * block device, as an llong.
  --  

  --*
  -- * VIR_DOMAIN_BLOCK_STATS_READ_REQ:
  -- *
  -- * Macro represents the total read requests of the
  -- * block device, as an llong.
  --  

  --*
  -- * VIR_DOMAIN_BLOCK_STATS_READ_TOTAL_TIMES:
  -- *
  -- * Macro represents the total time spend on cache reads in
  -- * nano-seconds of the block device, as an llong.
  --  

  --*
  -- * VIR_DOMAIN_BLOCK_STATS_WRITE_BYTES:
  -- *
  -- * Macro represents the total number of write bytes of the
  -- * block device, as an llong.
  --  

  --*
  -- * VIR_DOMAIN_BLOCK_STATS_WRITE_REQ:
  -- *
  -- * Macro represents the total write requests of the
  -- * block device, as an llong.
  --  

  --*
  -- * VIR_DOMAIN_BLOCK_STATS_WRITE_TOTAL_TIMES:
  -- *
  -- * Macro represents the total time spend on cache writes in
  -- * nano-seconds of the block device, as an llong.
  --  

  --*
  -- * VIR_DOMAIN_BLOCK_STATS_FLUSH_REQ:
  -- *
  -- * Macro represents the total flush requests of the
  -- * block device, as an llong.
  --  

  --*
  -- * VIR_DOMAIN_BLOCK_STATS_FLUSH_TOTAL_TIMES:
  -- *
  -- * Macro represents the total time spend on cache flushing in
  -- * nano-seconds of the block device, as an llong.
  --  

  --*
  -- * VIR_DOMAIN_BLOCK_STATS_ERRS:
  -- *
  -- * In Xen this returns the mysterious 'oo_req', as an llong.
  --  

  --*
  -- * virDomainInterfaceStats:
  -- *
  -- * Network interface stats for virDomainInterfaceStats.
  -- *
  -- * Hypervisors may return a field set to ((long long)-1) which indicates
  -- * that the hypervisor does not support that statistic.
  -- *
  -- * NB. Here 'long long' means 64 bit integer.
  --  

   type u_virDomainInterfaceStats;
   subtype virDomainInterfaceStatsStruct is u_virDomainInterfaceStats;

   type u_virDomainInterfaceStats is record
      rx_bytes : aliased Long_Long_Integer;  -- /usr/include/libvirt/libvirt-domain.h:565
      rx_packets : aliased Long_Long_Integer;  -- /usr/include/libvirt/libvirt-domain.h:566
      rx_errs : aliased Long_Long_Integer;  -- /usr/include/libvirt/libvirt-domain.h:567
      rx_drop : aliased Long_Long_Integer;  -- /usr/include/libvirt/libvirt-domain.h:568
      tx_bytes : aliased Long_Long_Integer;  -- /usr/include/libvirt/libvirt-domain.h:569
      tx_packets : aliased Long_Long_Integer;  -- /usr/include/libvirt/libvirt-domain.h:570
      tx_errs : aliased Long_Long_Integer;  -- /usr/include/libvirt/libvirt-domain.h:571
      tx_drop : aliased Long_Long_Integer;  -- /usr/include/libvirt/libvirt-domain.h:572
   end record;
   pragma Convention (C_Pass_By_Copy, u_virDomainInterfaceStats);  -- /usr/include/libvirt/libvirt-domain.h:564

  --*
  -- * virDomainInterfaceStatsPtr:
  -- *
  -- * A pointer to a virDomainInterfaceStats structure
  --  

   type virDomainInterfaceStatsPtr is access all virDomainInterfaceStatsStruct;  -- /usr/include/libvirt/libvirt-domain.h:580

  --*
  -- * Memory Statistics Tags:
  --  

  -- The total amount of data read from swap space (in kB).  
  -- The total amount of memory written out to swap space (in kB).  
  --     * Page faults occur when a process makes a valid access to virtual memory
  --     * that is not available.  When servicing the page fault, if disk IO is
  --     * required, it is considered a major fault.  If not, it is a minor fault.
  --     * These are expressed as the number of faults that have occurred.
  --      

  --     * The amount of memory left completely unused by the system.  Memory that
  --     * is available but used for reclaimable caches should NOT be reported as
  --     * free.  This value is expressed in kB.
  --      

  --     * The total amount of usable memory as seen by the domain.  This value
  --     * may be less than the amount of memory assigned to the domain if a
  --     * balloon driver is in use or if the guest OS does not initialize all
  --     * assigned pages.  This value is expressed in kB.
  --      

  -- Current balloon value (in KB).  
  -- Resident Set Size of the process running the domain. This value
  --     * is in kB  

  --     * How much the balloon can be inflated without pushing the guest system
  --     * to swap, corresponds to 'Available' in /proc/meminfo
  --      

  -- Timestamp of the last update of statistics, in seconds.  
  --     * The number of statistics supported by this version of the interface.
  --     * To add new statistics, add them to the enum and increase this value.
  --      

   type virDomainMemoryStatTags is 
     (VIR_DOMAIN_MEMORY_STAT_SWAP_IN,
      VIR_DOMAIN_MEMORY_STAT_SWAP_OUT,
      VIR_DOMAIN_MEMORY_STAT_MAJOR_FAULT,
      VIR_DOMAIN_MEMORY_STAT_MINOR_FAULT,
      VIR_DOMAIN_MEMORY_STAT_UNUSED,
      VIR_DOMAIN_MEMORY_STAT_AVAILABLE,
      VIR_DOMAIN_MEMORY_STAT_ACTUAL_BALLOON,
      VIR_DOMAIN_MEMORY_STAT_RSS,
      VIR_DOMAIN_MEMORY_STAT_USABLE,
      VIR_DOMAIN_MEMORY_STAT_LAST_UPDATE,
      VIR_DOMAIN_MEMORY_STAT_NR);
   pragma Convention (C, virDomainMemoryStatTags);  -- /usr/include/libvirt/libvirt-domain.h:640

   type u_virDomainMemoryStat;
   subtype virDomainMemoryStatStruct is u_virDomainMemoryStat;

   type u_virDomainMemoryStat is record
      tag : aliased int;  -- /usr/include/libvirt/libvirt-domain.h:645
      val : aliased Extensions.unsigned_long_long;  -- /usr/include/libvirt/libvirt-domain.h:646
   end record;
   pragma Convention (C_Pass_By_Copy, u_virDomainMemoryStat);  -- /usr/include/libvirt/libvirt-domain.h:644

   type virDomainMemoryStatPtr is access all virDomainMemoryStatStruct;  -- /usr/include/libvirt/libvirt-domain.h:649

  -- Domain core dump flags.  
  -- crash after dump  
  -- live dump  
  -- avoid file system cache pollution  
  -- reset domain after dump finishes  
  -- use dump-guest-memory  
   subtype virDomainCoreDumpFlags is unsigned;
   VIR_DUMP_CRASH : constant virDomainCoreDumpFlags := 1;
   VIR_DUMP_LIVE : constant virDomainCoreDumpFlags := 2;
   VIR_DUMP_BYPASS_CACHE : constant virDomainCoreDumpFlags := 4;
   VIR_DUMP_RESET : constant virDomainCoreDumpFlags := 8;
   VIR_DUMP_MEMORY_ONLY : constant virDomainCoreDumpFlags := 16;  -- /usr/include/libvirt/libvirt-domain.h:659

  --*
  -- * virDomainCoreDumpFormat:
  -- *
  -- * Values for specifying different formats of domain core dumps.
  --  

  -- dump guest memory in raw format  
  -- kdump-compressed format, with
  --                                               * zlib compression  

  -- kdump-compressed format, with
  --                                               * lzo compression  

  -- kdump-compressed format, with
  --                                               * snappy compression  

  --     * NB: this enum value will increase over time as new events are
  --     * added to the libvirt API. It reflects the last state supported
  --     * by this version of the libvirt API.
  --      

   type virDomainCoreDumpFormat is 
     (VIR_DOMAIN_CORE_DUMP_FORMAT_RAW,
      VIR_DOMAIN_CORE_DUMP_FORMAT_KDUMP_ZLIB,
      VIR_DOMAIN_CORE_DUMP_FORMAT_KDUMP_LZO,
      VIR_DOMAIN_CORE_DUMP_FORMAT_KDUMP_SNAPPY);
   pragma Convention (C, virDomainCoreDumpFormat);  -- /usr/include/libvirt/libvirt-domain.h:682

  -- Domain migration flags.  
  -- Do not pause the domain during migration. The domain's memory will
  --     * be transferred to the destination host while the domain is running.
  --     * The migration may never converge if the domain is changing its memory
  --     * faster then it can be transferred. The domain can be manually paused
  --     * anytime during migration using virDomainSuspend.
  --      

  -- Tell the source libvirtd to connect directly to the destination host.
  --     * Without this flag the client (e.g., virsh) connects to both hosts and
  --     * controls the migration process. In peer-to-peer mode, the source
  --     * libvirtd controls the migration by calling the destination daemon
  --     * directly.
  --      

  -- Tunnel migration data over libvirtd connection. Without this flag the
  --     * source hypervisor sends migration data directly to the destination
  --     * hypervisor. This flag can only be used when VIR_MIGRATE_PEER2PEER is
  --     * set as well.
  --     *
  --     * Note the less-common spelling that we're stuck with:
  --     * VIR_MIGRATE_TUNNELLED should be VIR_MIGRATE_TUNNELED.
  --      

  -- Define the domain as persistent on the destination host after successful
  --     * migration. If the domain was persistent on the source host and
  --     * VIR_MIGRATE_UNDEFINE_SOURCE is not used, it will end up persistent on
  --     * both hosts.
  --      

  -- Undefine the domain on the source host once migration successfully
  --     * finishes.
  --      

  -- Leave the domain suspended on the destination host. virDomainResume (on
  --     * the virDomainPtr returned by the migration API) has to be called
  --     * explicitly to resume domain's virtual CPUs.
  --      

  -- Migrate full disk images in addition to domain's memory. By default
  --     * only non-shared non-readonly disk images are transferred. The
  --     * VIR_MIGRATE_PARAM_MIGRATE_DISKS parameter can be used to specify which
  --     * disks should be migrated.
  --     *
  --     * This flag and VIR_MIGRATE_NON_SHARED_INC are mutually exclusive.
  --      

  -- Migrate disk images in addition to domain's memory. This is similar to
  --     * VIR_MIGRATE_NON_SHARED_DISK, but only the top level of each disk's
  --     * backing chain is copied. That is, the rest of the backing chain is
  --     * expected to be present on the destination and to be exactly the same as
  --     * on the source host.
  --     *
  --     * This flag and VIR_MIGRATE_NON_SHARED_DISK are mutually exclusive.
  --      

  -- Protect against domain configuration changes during the migration
  --     * process. This flag is used automatically when both sides support it.
  --     * Explicitly setting this flag will cause migration to fail if either the
  --     * source or the destination does not support it.
  --      

  -- Force migration even if it is considered unsafe. In some cases libvirt
  --     * may refuse to migrate the domain because doing so may lead to potential
  --     * problems such as data corruption, and thus the migration is considered
  --     * unsafe. For a QEMU domain this may happen if the domain uses disks
  --     * without explicitly setting cache mode to "none". Migrating such domains
  --     * is unsafe unless the disk images are stored on coherent clustered
  --     * filesystem, such as GFS2 or GPFS.
  --      

  -- Migrate a domain definition without starting the domain on the
  --     * destination and without stopping it on the source host. Offline
  --     * migration requires VIR_MIGRATE_PERSIST_DEST to be set.
  --     *
  --     * Offline migration may not copy disk storage or any other file based
  --     * storage (such as UEFI variables).
  --      

  -- Compress migration data. The compression methods can be specified using
  --     * VIR_MIGRATE_PARAM_COMPRESSION. A hypervisor default method will be used
  --     * if this parameter is omitted. Individual compression methods can be
  --     * tuned via their specific VIR_MIGRATE_PARAM_COMPRESSION_* parameters.
  --      

  -- Cancel migration if a soft error (such as I/O error) happens during
  --     * migration.
  --      

  -- Enable algorithms that ensure a live migration will eventually converge.
  --     * This usually means the domain will be slowed down to make sure it does
  --     * not change its memory faster than a hypervisor can transfer the changed
  --     * memory to the destination host. VIR_MIGRATE_PARAM_AUTO_CONVERGE_*
  --     * parameters can be used to tune the algorithm.
  --      

  -- This flag can be used with RDMA migration (i.e., when
  --     * VIR_MIGRATE_PARAM_URI starts with "rdma://") to tell the hypervisor
  --     * to pin all domain's memory at once before migration starts rather then
  --     * letting it pin memory pages as needed. This means that all memory pages
  --     * belonging to the domain will be locked in host's memory and the host
  --     * will not be allowed to swap them out.
  --     *
  --     * For QEMU/KVM this requires hard_limit memory tuning element (in the
  --     * domain XML) to be used and set to the maximum memory configured for the
  --     * domain plus any memory consumed by the QEMU process itself. Beware of
  --     * setting the memory limit too high (and thus allowing the domain to lock
  --     * most of the host's memory). Doing so may be dangerous to both the
  --     * domain and the host itself since the host's kernel may run out of
  --     * memory.
  --      

  -- Setting the VIR_MIGRATE_POSTCOPY flag tells libvirt to enable post-copy
  --     * migration. However, the migration will start normally and
  --     * virDomainMigrateStartPostCopy needs to be called to switch it into the
  --     * post-copy mode. See virDomainMigrateStartPostCopy for more details.
  --      

   subtype virDomainMigrateFlags is unsigned;
   VIR_MIGRATE_LIVE : constant virDomainMigrateFlags := 1;
   VIR_MIGRATE_PEER2PEER : constant virDomainMigrateFlags := 2;
   VIR_MIGRATE_TUNNELLED : constant virDomainMigrateFlags := 4;
   VIR_MIGRATE_PERSIST_DEST : constant virDomainMigrateFlags := 8;
   VIR_MIGRATE_UNDEFINE_SOURCE : constant virDomainMigrateFlags := 16;
   VIR_MIGRATE_PAUSED : constant virDomainMigrateFlags := 32;
   VIR_MIGRATE_NON_SHARED_DISK : constant virDomainMigrateFlags := 64;
   VIR_MIGRATE_NON_SHARED_INC : constant virDomainMigrateFlags := 128;
   VIR_MIGRATE_CHANGE_PROTECTION : constant virDomainMigrateFlags := 256;
   VIR_MIGRATE_UNSAFE : constant virDomainMigrateFlags := 512;
   VIR_MIGRATE_OFFLINE : constant virDomainMigrateFlags := 1024;
   VIR_MIGRATE_COMPRESSED : constant virDomainMigrateFlags := 2048;
   VIR_MIGRATE_ABORT_ON_ERROR : constant virDomainMigrateFlags := 4096;
   VIR_MIGRATE_AUTO_CONVERGE : constant virDomainMigrateFlags := 8192;
   VIR_MIGRATE_RDMA_PIN_ALL : constant virDomainMigrateFlags := 16384;
   VIR_MIGRATE_POSTCOPY : constant virDomainMigrateFlags := 32768;  -- /usr/include/libvirt/libvirt-domain.h:818

  --*
  -- * VIR_MIGRATE_PARAM_URI:
  -- *
  -- * virDomainMigrate* params field: URI to use for initiating domain migration
  -- * as VIR_TYPED_PARAM_STRING. It takes a hypervisor specific format. The
  -- * uri_transports element of the hypervisor capabilities XML includes details
  -- * of the supported URI schemes. When omitted libvirt will auto-generate
  -- * suitable default URI. It is typically only necessary to specify this URI if
  -- * the destination host has multiple interfaces and a specific interface is
  -- * required to transmit migration data.
  -- *
  -- * This filed may not be used when VIR_MIGRATE_TUNNELLED flag is set.
  --  

  --*
  -- * VIR_MIGRATE_PARAM_DEST_NAME:
  -- *
  -- * virDomainMigrate* params field: the name to be used for the domain on the
  -- * destination host as VIR_TYPED_PARAM_STRING. Omitting this parameter keeps
  -- * the domain name the same. This field is only allowed to be used with
  -- * hypervisors that support domain renaming during migration.
  --  

  --*
  -- * VIR_MIGRATE_PARAM_DEST_XML:
  -- *
  -- * virDomainMigrate* params field: the new configuration to be used for the
  -- * domain on the destination host as VIR_TYPED_PARAM_STRING. The configuration
  -- * must include an identical set of virtual devices, to ensure a stable guest
  -- * ABI across migration. Only parameters related to host side configuration
  -- * can be changed in the XML. Hypervisors which support this field will forbid
  -- * migration if the provided XML would cause a change in the guest ABI. This
  -- * field cannot be used to rename the domain during migration (use
  -- * VIR_MIGRATE_PARAM_DEST_NAME field for that purpose). Domain name in the
  -- * destination XML must match the original domain name.
  -- *
  -- * Omitting this parameter keeps the original domain configuration. Using this
  -- * field with hypervisors that do not support changing domain configuration
  -- * during migration will result in a failure.
  --  

  --*
  -- * VIR_MIGRATE_PARAM_PERSIST_XML:
  -- *
  -- * virDomainMigrate* params field: the new persistent configuration to be used
  -- * for the domain on the destination host as VIR_TYPED_PARAM_STRING.
  -- * This field cannot be used to rename the domain during migration (use
  -- * VIR_MIGRATE_PARAM_DEST_NAME field for that purpose). Domain name in the
  -- * destination XML must match the original domain name.
  -- *
  -- * Omitting this parameter keeps the original domain persistent configuration.
  -- * Using this field with hypervisors that do not support changing domain
  -- * configuration during migration will result in a failure.
  --  

  --*
  -- * VIR_MIGRATE_PARAM_BANDWIDTH:
  -- *
  -- * virDomainMigrate* params field: the maximum bandwidth (in MiB/s) that will
  -- * be used for migration as VIR_TYPED_PARAM_ULLONG. If set to 0 or omitted,
  -- * libvirt will choose a suitable default. Some hypervisors do not support this
  -- * feature and will return an error if this field is used and is not 0.
  --  

  --*
  -- * VIR_MIGRATE_PARAM_GRAPHICS_URI:
  -- *
  -- * virDomainMigrate* params field: URI to use for migrating client's connection
  -- * to domain's graphical console as VIR_TYPED_PARAM_STRING. If specified, the
  -- * client will be asked to automatically reconnect using these parameters
  -- * instead of the automatically computed ones. This can be useful if, e.g., the
  -- * client does not have a direct access to the network virtualization hosts are
  -- * connected to and needs to connect through a proxy. The URI is formed as
  -- * follows:
  -- *
  -- *      protocol://hostname[:port]/[?parameters]
  -- *
  -- * where protocol is either "spice" or "vnc" and parameters is a list of
  -- * protocol specific parameters separated by '&'. Currently recognized
  -- * parameters are "tlsPort" and "tlsSubject". For example,
  -- *
  -- *      spice://target.host.com:1234/?tlsPort=4567
  --  

  --*
  -- * VIR_MIGRATE_PARAM_LISTEN_ADDRESS:
  -- *
  -- * virDomainMigrate* params field: The listen address that hypervisor on the
  -- * destination side should bind to for incoming migration. Both IPv4 and IPv6
  -- * addresses are accepted as well as hostnames (the resolving is done on
  -- * destination). Some hypervisors do not support this feature and will return
  -- * an error if this field is used.
  --  

  --*
  -- * VIR_MIGRATE_PARAM_MIGRATE_DISKS:
  -- *
  -- * virDomainMigrate* params multiple field: The multiple values that list
  -- * the block devices to be migrated. At the moment this is only supported
  -- * by the QEMU driver but not for the tunnelled migration.
  --  

  --*
  -- * VIR_MIGRATE_PARAM_DISKS_PORT:
  -- *
  -- * virDomainMigrate* params field: port that destination server should use
  -- * for incoming disks migration. Type is VIR_TYPED_PARAM_INT. If set to 0 or
  -- * omitted, libvirt will choose a suitable default. At the moment this is only
  -- * supported by the QEMU driver.
  --  

  --*
  -- * VIR_MIGRATE_PARAM_COMPRESSION:
  -- *
  -- * virDomainMigrate* params multiple field: name of the method used to
  -- * compress migration traffic. Supported compression methods: xbzrle, mt.
  -- * The parameter may be specified multiple times if more than one method
  -- * should be used.
  --  

  --*
  -- * VIR_MIGRATE_PARAM_COMPRESSION_MT_LEVEL:
  -- *
  -- * virDomainMigrate* params field: the level of compression for multithread
  -- * compression as VIR_TYPED_PARAM_INT. Accepted values are in range 0-9.
  -- * 0 is no compression, 1 is maximum speed and 9 is maximum compression.
  --  

  --*
  -- * VIR_MIGRATE_PARAM_COMPRESSION_MT_THREADS:
  -- *
  -- * virDomainMigrate* params field: the number of compression threads for
  -- * multithread compression as VIR_TYPED_PARAM_INT.
  --  

  --*
  -- * VIR_MIGRATE_PARAM_COMPRESSION_MT_DTHREADS:
  -- *
  -- * virDomainMigrate* params field: the number of decompression threads for
  -- * multithread compression as VIR_TYPED_PARAM_INT.
  --  

  --*
  -- * VIR_MIGRATE_PARAM_COMPRESSION_XBZRLE_CACHE:
  -- *
  -- * virDomainMigrate* params field: the size of page cache for xbzrle
  -- * compression as VIR_TYPED_PARAM_ULLONG.
  --  

  --*
  -- * VIR_MIGRATE_PARAM_AUTO_CONVERGE_INITIAL:
  -- *
  -- * virDomainMigrate* params field: the initial percentage guest CPUs are
  -- * throttled to when auto-convergence decides migration is not converging.
  -- * As VIR_TYPED_PARAM_INT.
  --  

  --*
  -- * VIR_MIGRATE_PARAM_AUTO_CONVERGE_INCREMENT:
  -- *
  -- * virDomainMigrate* params field: the increment added to
  -- * VIR_MIGRATE_PARAM_AUTO_CONVERGE_INITIAL whenever the hypervisor decides
  -- * the current rate is not enough to ensure convergence of the migration.
  -- * As VIR_TYPED_PARAM_INT.
  --  

  -- Domain migration.  
   function virDomainMigrate
     (domain : virDomainPtr;
      dconn : virConnectPtr;
      flags : unsigned_long;
      dname : Interfaces.C.Strings.chars_ptr;
      uri : Interfaces.C.Strings.chars_ptr;
      bandwidth : unsigned_long) return virDomainPtr;  -- /usr/include/libvirt/libvirt-domain.h:1004
   pragma Import (C, virDomainMigrate, "virDomainMigrate");

   function virDomainMigrate2
     (domain : virDomainPtr;
      dconn : virConnectPtr;
      dxml : Interfaces.C.Strings.chars_ptr;
      flags : unsigned_long;
      dname : Interfaces.C.Strings.chars_ptr;
      uri : Interfaces.C.Strings.chars_ptr;
      bandwidth : unsigned_long) return virDomainPtr;  -- /usr/include/libvirt/libvirt-domain.h:1007
   pragma Import (C, virDomainMigrate2, "virDomainMigrate2");

   function virDomainMigrate3
     (domain : virDomainPtr;
      dconn : virConnectPtr;
      params : virTypedParameterPtr;
      nparams : unsigned;
      flags : unsigned) return virDomainPtr;  -- /usr/include/libvirt/libvirt-domain.h:1011
   pragma Import (C, virDomainMigrate3, "virDomainMigrate3");

   function virDomainMigrateToURI
     (domain : virDomainPtr;
      duri : Interfaces.C.Strings.chars_ptr;
      flags : unsigned_long;
      dname : Interfaces.C.Strings.chars_ptr;
      bandwidth : unsigned_long) return int;  -- /usr/include/libvirt/libvirt-domain.h:1017
   pragma Import (C, virDomainMigrateToURI, "virDomainMigrateToURI");

   function virDomainMigrateToURI2
     (domain : virDomainPtr;
      dconnuri : Interfaces.C.Strings.chars_ptr;
      miguri : Interfaces.C.Strings.chars_ptr;
      dxml : Interfaces.C.Strings.chars_ptr;
      flags : unsigned_long;
      dname : Interfaces.C.Strings.chars_ptr;
      bandwidth : unsigned_long) return int;  -- /usr/include/libvirt/libvirt-domain.h:1021
   pragma Import (C, virDomainMigrateToURI2, "virDomainMigrateToURI2");

   function virDomainMigrateToURI3
     (domain : virDomainPtr;
      dconnuri : Interfaces.C.Strings.chars_ptr;
      params : virTypedParameterPtr;
      nparams : unsigned;
      flags : unsigned) return int;  -- /usr/include/libvirt/libvirt-domain.h:1028
   pragma Import (C, virDomainMigrateToURI3, "virDomainMigrateToURI3");

   function virDomainMigrateSetMaxDowntime
     (domain : virDomainPtr;
      downtime : Extensions.unsigned_long_long;
      flags : unsigned) return int;  -- /usr/include/libvirt/libvirt-domain.h:1034
   pragma Import (C, virDomainMigrateSetMaxDowntime, "virDomainMigrateSetMaxDowntime");

   function virDomainMigrateGetCompressionCache
     (domain : virDomainPtr;
      cacheSize : access Extensions.unsigned_long_long;
      flags : unsigned) return int;  -- /usr/include/libvirt/libvirt-domain.h:1038
   pragma Import (C, virDomainMigrateGetCompressionCache, "virDomainMigrateGetCompressionCache");

   function virDomainMigrateSetCompressionCache
     (domain : virDomainPtr;
      cacheSize : Extensions.unsigned_long_long;
      flags : unsigned) return int;  -- /usr/include/libvirt/libvirt-domain.h:1041
   pragma Import (C, virDomainMigrateSetCompressionCache, "virDomainMigrateSetCompressionCache");

   function virDomainMigrateSetMaxSpeed
     (domain : virDomainPtr;
      bandwidth : unsigned_long;
      flags : unsigned) return int;  -- /usr/include/libvirt/libvirt-domain.h:1045
   pragma Import (C, virDomainMigrateSetMaxSpeed, "virDomainMigrateSetMaxSpeed");

   function virDomainMigrateGetMaxSpeed
     (domain : virDomainPtr;
      bandwidth : access unsigned_long;
      flags : unsigned) return int;  -- /usr/include/libvirt/libvirt-domain.h:1049
   pragma Import (C, virDomainMigrateGetMaxSpeed, "virDomainMigrateGetMaxSpeed");

   function virDomainMigrateStartPostCopy (domain : virDomainPtr; flags : unsigned) return int;  -- /usr/include/libvirt/libvirt-domain.h:1053
   pragma Import (C, virDomainMigrateStartPostCopy, "virDomainMigrateStartPostCopy");

   function virConnectGetDomainCapabilities
     (conn : virConnectPtr;
      emulatorbin : Interfaces.C.Strings.chars_ptr;
      arch : Interfaces.C.Strings.chars_ptr;
      machine : Interfaces.C.Strings.chars_ptr;
      virttype : Interfaces.C.Strings.chars_ptr;
      flags : unsigned) return Interfaces.C.Strings.chars_ptr;  -- /usr/include/libvirt/libvirt-domain.h:1056
   pragma Import (C, virConnectGetDomainCapabilities, "virConnectGetDomainCapabilities");

  -- * Gather list of running domains
  --  

   function virConnectListDomains
     (conn : virConnectPtr;
      ids : access int;
      maxids : int) return int;  -- /usr/include/libvirt/libvirt-domain.h:1066
   pragma Import (C, virConnectListDomains, "virConnectListDomains");

  -- * Number of domains
  --  

   function virConnectNumOfDomains (conn : virConnectPtr) return int;  -- /usr/include/libvirt/libvirt-domain.h:1073
   pragma Import (C, virConnectNumOfDomains, "virConnectNumOfDomains");

  -- * Get connection from domain.
  --  

   function virDomainGetConnect (domain : virDomainPtr) return virConnectPtr;  -- /usr/include/libvirt/libvirt-domain.h:1079
   pragma Import (C, virDomainGetConnect, "virDomainGetConnect");

  -- * Domain creation and destruction
  --  

   function virDomainCreateXML
     (conn : VirConnectPtr;
      xmlDesc : Interfaces.C.Strings.chars_ptr;
      flags : unsigned) return virDomainPtr;  -- /usr/include/libvirt/libvirt-domain.h:1085
   pragma Import (C, virDomainCreateXML, "virDomainCreateXML");

   function virDomainCreateXMLWithFiles
     (conn : VirConnectPtr;
      xmlDesc : Interfaces.C.Strings.chars_ptr;
      nfiles : unsigned;
      files : access int;
      flags : unsigned) return virDomainPtr;  -- /usr/include/libvirt/libvirt-domain.h:1088
   pragma Import (C, virDomainCreateXMLWithFiles, "virDomainCreateXMLWithFiles");

   function virDomainLookupByName (conn : VirConnectPtr; name : Interfaces.C.Strings.chars_ptr) return virDomainPtr;  -- /usr/include/libvirt/libvirt-domain.h:1093
   pragma Import (C, virDomainLookupByName, "virDomainLookupByName");

   function virDomainLookupByID (conn : VirConnectPtr; id : int) return virDomainPtr;  -- /usr/include/libvirt/libvirt-domain.h:1095
   pragma Import (C, virDomainLookupByID, "virDomainLookupByID");

   function virDomainLookupByUUID (conn : VirConnectPtr; uuid : access unsigned_char) return virDomainPtr;  -- /usr/include/libvirt/libvirt-domain.h:1097
   pragma Import (C, virDomainLookupByUUID, "virDomainLookupByUUID");

   function virDomainLookupByUUIDString (conn : VirConnectPtr; uuid : Interfaces.C.Strings.chars_ptr) return virDomainPtr;  -- /usr/include/libvirt/libvirt-domain.h:1099
   pragma Import (C, virDomainLookupByUUIDString, "virDomainLookupByUUIDString");

  -- hypervisor choice  
  -- Send ACPI event  
  -- Use guest agent  
  -- Use initctl  
  -- Send a signal  
  -- Use paravirt guest control  
   subtype virDomainShutdownFlagValues is unsigned;
   VIR_DOMAIN_SHUTDOWN_DEFAULT : constant virDomainShutdownFlagValues := 0;
   VIR_DOMAIN_SHUTDOWN_ACPI_POWER_BTN : constant virDomainShutdownFlagValues := 1;
   VIR_DOMAIN_SHUTDOWN_GUEST_AGENT : constant virDomainShutdownFlagValues := 2;
   VIR_DOMAIN_SHUTDOWN_INITCTL : constant virDomainShutdownFlagValues := 4;
   VIR_DOMAIN_SHUTDOWN_SIGNAL : constant virDomainShutdownFlagValues := 8;
   VIR_DOMAIN_SHUTDOWN_PARAVIRT : constant virDomainShutdownFlagValues := 16;  -- /usr/include/libvirt/libvirt-domain.h:1109

   function virDomainShutdown (domain : virDomainPtr) return int;  -- /usr/include/libvirt/libvirt-domain.h:1111
   pragma Import (C, virDomainShutdown, "virDomainShutdown");

   function virDomainShutdownFlags (domain : virDomainPtr; flags : unsigned) return int;  -- /usr/include/libvirt/libvirt-domain.h:1112
   pragma Import (C, virDomainShutdownFlags, "virDomainShutdownFlags");

  -- hypervisor choice  
  -- Send ACPI event  
  -- Use guest agent  
  -- Use initctl  e
  -- Send a signal  
  -- Use paravirt guest control  
   subtype virDomainRebootFlagValues is unsigned;
   VIR_DOMAIN_REBOOT_DEFAULT : constant virDomainRebootFlagValues := 0;
   VIR_DOMAIN_REBOOT_ACPI_POWER_BTN : constant virDomainRebootFlagValues := 1;
   VIR_DOMAIN_REBOOT_GUEST_AGENT : constant virDomainRebootFlagValues := 2;
   VIR_DOMAIN_REBOOT_INITCTL : constant virDomainRebootFlagValues := 4;
   VIR_DOMAIN_REBOOT_SIGNAL : constant virDomainRebootFlagValues := 8;
   VIR_DOMAIN_REBOOT_PARAVIRT : constant virDomainRebootFlagValues := 16;  -- /usr/include/libvirt/libvirt-domain.h:1122

   function virDomainReboot (domain : virDomainPtr; flags : unsigned) return int;  -- /usr/include/libvirt/libvirt-domain.h:1124
   pragma Import (C, virDomainReboot, "virDomainReboot");

   function virDomainReset (domain : virDomainPtr; flags : unsigned) return int;  -- /usr/include/libvirt/libvirt-domain.h:1126
   pragma Import (C, virDomainReset, "virDomainReset");

   function virDomainDestroy (domain : virDomainPtr) return int;  -- /usr/include/libvirt/libvirt-domain.h:1129
   pragma Import (C, virDomainDestroy, "virDomainDestroy");

  --*
  -- * virDomainDestroyFlagsValues:
  -- *
  -- * Flags used to provide specific behaviour to the
  -- * virDomainDestroyFlags() function
  --  

  -- Default behavior - could lead to data loss!!  
  -- only SIGTERM, no SIGKILL  
   type virDomainDestroyFlagsValues is 
     (VIR_DOMAIN_DESTROY_DEFAULT,
      VIR_DOMAIN_DESTROY_GRACEFUL);
   pragma Convention (C, virDomainDestroyFlagsValues);  -- /usr/include/libvirt/libvirt-domain.h:1140

   function virDomainDestroyFlags (domain : virDomainPtr; flags : unsigned) return int;  -- /usr/include/libvirt/libvirt-domain.h:1142
   pragma Import (C, virDomainDestroyFlags, "virDomainDestroyFlags");

   function virDomainRef (domain : virDomainPtr) return int;  -- /usr/include/libvirt/libvirt-domain.h:1144
   pragma Import (C, virDomainRef, "virDomainRef");

   function virDomainFree (domain : virDomainPtr) return int;  -- /usr/include/libvirt/libvirt-domain.h:1145
   pragma Import (C, virDomainFree, "virDomainFree");

  -- * Domain suspend/resume
  --  

   function virDomainSuspend (domain : virDomainPtr) return int;  -- /usr/include/libvirt/libvirt-domain.h:1150
   pragma Import (C, virDomainSuspend, "virDomainSuspend");

   function virDomainResume (domain : virDomainPtr) return int;  -- /usr/include/libvirt/libvirt-domain.h:1151
   pragma Import (C, virDomainResume, "virDomainResume");

   function virDomainPMSuspendForDuration
     (domain : virDomainPtr;
      target : unsigned;
      duration : Extensions.unsigned_long_long;
      flags : unsigned) return int;  -- /usr/include/libvirt/libvirt-domain.h:1152
   pragma Import (C, virDomainPMSuspendForDuration, "virDomainPMSuspendForDuration");

   function virDomainPMWakeup (domain : virDomainPtr; flags : unsigned) return int;  -- /usr/include/libvirt/libvirt-domain.h:1156
   pragma Import (C, virDomainPMWakeup, "virDomainPMWakeup");

  -- * Domain save/restore
  --  

  --*
  -- * virDomainSaveRestoreFlags:
  -- * Flags for use in virDomainSaveFlags(), virDomainManagedSave(),
  -- * virDomainRestoreFlags(), and virDomainSaveImageDefineXML().  Not all
  -- * flags apply to all these functions.
  --  

  -- Avoid file system cache pollution  
  -- Favor running over paused  
  -- Favor paused over running  
   subtype virDomainSaveRestoreFlags is unsigned;
   VIR_DOMAIN_SAVE_BYPASS_CACHE : constant virDomainSaveRestoreFlags := 1;
   VIR_DOMAIN_SAVE_RUNNING : constant virDomainSaveRestoreFlags := 2;
   VIR_DOMAIN_SAVE_PAUSED : constant virDomainSaveRestoreFlags := 4;  -- /usr/include/libvirt/libvirt-domain.h:1172

   function virDomainSave (domain : virDomainPtr; to : Interfaces.C.Strings.chars_ptr) return int;  -- /usr/include/libvirt/libvirt-domain.h:1174
   pragma Import (C, virDomainSave, "virDomainSave");

   function virDomainSaveFlags
     (domain : virDomainPtr;
      to : Interfaces.C.Strings.chars_ptr;
      dxml : Interfaces.C.Strings.chars_ptr;
      flags : unsigned) return int;  -- /usr/include/libvirt/libvirt-domain.h:1176
   pragma Import (C, virDomainSaveFlags, "virDomainSaveFlags");

   function virDomainRestore (conn : VirConnectPtr; from : Interfaces.C.Strings.chars_ptr) return int;  -- /usr/include/libvirt/libvirt-domain.h:1180
   pragma Import (C, virDomainRestore, "virDomainRestore");

   function virDomainRestoreFlags
     (conn : VirConnectPtr;
      from : Interfaces.C.Strings.chars_ptr;
      dxml : Interfaces.C.Strings.chars_ptr;
      flags : unsigned) return int;  -- /usr/include/libvirt/libvirt-domain.h:1182
   pragma Import (C, virDomainRestoreFlags, "virDomainRestoreFlags");

   function virDomainSaveImageGetXMLDesc
     (conn : VirConnectPtr;
      file : Interfaces.C.Strings.chars_ptr;
      flags : unsigned) return Interfaces.C.Strings.chars_ptr;  -- /usr/include/libvirt/libvirt-domain.h:1187
   pragma Import (C, virDomainSaveImageGetXMLDesc, "virDomainSaveImageGetXMLDesc");

   function virDomainSaveImageDefineXML
     (conn : VirConnectPtr;
      file : Interfaces.C.Strings.chars_ptr;
      dxml : Interfaces.C.Strings.chars_ptr;
      flags : unsigned) return int;  -- /usr/include/libvirt/libvirt-domain.h:1190
   pragma Import (C, virDomainSaveImageDefineXML, "virDomainSaveImageDefineXML");

  -- * Managed domain save
  --  

   function virDomainManagedSave (dom : virDomainPtr; flags : unsigned) return int;  -- /usr/include/libvirt/libvirt-domain.h:1198
   pragma Import (C, virDomainManagedSave, "virDomainManagedSave");

   function virDomainHasManagedSaveImage (dom : virDomainPtr; flags : unsigned) return int;  -- /usr/include/libvirt/libvirt-domain.h:1200
   pragma Import (C, virDomainHasManagedSaveImage, "virDomainHasManagedSaveImage");

   function virDomainManagedSaveRemove (dom : virDomainPtr; flags : unsigned) return int;  -- /usr/include/libvirt/libvirt-domain.h:1202
   pragma Import (C, virDomainManagedSaveRemove, "virDomainManagedSaveRemove");

  -- * Domain core dump
  --  

   function virDomainCoreDump
     (domain : virDomainPtr;
      to : Interfaces.C.Strings.chars_ptr;
      flags : unsigned) return int;  -- /usr/include/libvirt/libvirt-domain.h:1208
   pragma Import (C, virDomainCoreDump, "virDomainCoreDump");

  -- * Domain core dump with format specified
  --  

   function virDomainCoreDumpWithFormat
     (domain : virDomainPtr;
      to : Interfaces.C.Strings.chars_ptr;
      dumpformat : unsigned;
      flags : unsigned) return int;  -- /usr/include/libvirt/libvirt-domain.h:1215
   pragma Import (C, virDomainCoreDumpWithFormat, "virDomainCoreDumpWithFormat");

  -- * Screenshot of current domain console
  --  

   function virDomainScreenshot
     (domain : virDomainPtr;
      stream : virStreamPtr;
      screen : unsigned;
      flags : unsigned) return Interfaces.C.Strings.chars_ptr;  -- /usr/include/libvirt/libvirt-domain.h:1223
   pragma Import (C, virDomainScreenshot, "virDomainScreenshot");

  -- * Domain runtime information, and collecting CPU statistics
  --  

   function virDomainGetInfo (domain : virDomainPtr; info : virDomainInfoPtr) return int;  -- /usr/include/libvirt/libvirt-domain.h:1232
   pragma Import (C, virDomainGetInfo, "virDomainGetInfo");

   function virDomainGetState
     (domain : virDomainPtr;
      state : access int;
      reason : access int;
      flags : unsigned) return int;  -- /usr/include/libvirt/libvirt-domain.h:1234
   pragma Import (C, virDomainGetState, "virDomainGetState");

  --*
  -- * VIR_DOMAIN_CPU_STATS_CPUTIME:
  -- * cpu usage (sum of both vcpu and hypervisor usage) in nanoseconds,
  -- * as a ullong
  --  

  --*
  -- * VIR_DOMAIN_CPU_STATS_USERTIME:
  -- * cpu time charged to user instructions in nanoseconds, as a ullong
  --  

  --*
  -- * VIR_DOMAIN_CPU_STATS_SYSTEMTIME:
  -- * cpu time charged to system instructions in nanoseconds, as a ullong
  --  

  --*
  -- * VIR_DOMAIN_CPU_STATS_VCPUTIME:
  -- * vcpu usage in nanoseconds (cpu_time excluding hypervisor time),
  -- * as a ullong
  --  

   function virDomainGetCPUStats
     (domain : virDomainPtr;
      params : virTypedParameterPtr;
      nparams : unsigned;
      start_cpu : int;
      ncpus : unsigned;
      flags : unsigned) return int;  -- /usr/include/libvirt/libvirt-domain.h:1265
   pragma Import (C, virDomainGetCPUStats, "virDomainGetCPUStats");

   function virDomainGetControlInfo
     (domain : virDomainPtr;
      info : virDomainControlInfoPtr;
      flags : unsigned) return int;  -- /usr/include/libvirt/libvirt-domain.h:1272
   pragma Import (C, virDomainGetControlInfo, "virDomainGetControlInfo");

  -- * Return scheduler type in effect 'sedf', 'credit', 'linux'
  --  

   function virDomainGetSchedulerType (domain : virDomainPtr; nparams : access int) return Interfaces.C.Strings.chars_ptr;  -- /usr/include/libvirt/libvirt-domain.h:1279
   pragma Import (C, virDomainGetSchedulerType, "virDomainGetSchedulerType");

  -- Manage blkio parameters.   
  --*
  -- * VIR_DOMAIN_BLKIO_WEIGHT:
  -- *
  -- * Macro for the Blkio tunable weight: it represents the io weight
  -- * the guest can use, as a uint.
  --  

  --*
  -- * VIR_DOMAIN_BLKIO_DEVICE_WEIGHT:
  -- *
  -- * Macro for the blkio tunable weight_device: it represents the
  -- * per-device weight, as a string.  The string is parsed as a
  -- * series of /path/to/device,weight elements, separated by ','.
  --  

  --*
  -- * VIR_DOMAIN_BLKIO_DEVICE_READ_IOPS:
  -- *
  -- * Macro for the blkio tunable throttle.read_iops_device: it represents
  -- * the number of reading the block device per second, as a string. The
  -- * string is parsed as a series of /path/to/device, read_iops elements,
  -- * separated by ','.
  --  

  --*
  -- * VIR_DOMAIN_BLKIO_DEVICE_WRITE_IOPS:
  -- *
  -- * Macro for the blkio tunable throttle.write_iops_device: it represents
  -- * the number of writing the block device per second, as a string. The
  -- * string is parsed as a series of /path/to/device, write_iops elements,
  -- * separated by ','.
  --  

  --*
  -- * VIR_DOMAIN_BLKIO_DEVICE_READ_BPS:
  -- *
  -- * Macro for the blkio tunable throttle.read_iops_device: it represents
  -- * the bytes of reading the block device per second, as a string. The
  -- * string is parsed as a series of /path/to/device, read_bps elements,
  -- * separated by ','.
  --  

  --*
  -- * VIR_DOMAIN_BLKIO_DEVICE_WRITE_BPS:
  -- *
  -- * Macro for the blkio tunable throttle.read_iops_device: it represents
  -- * the number of reading the block device per second, as a string. The
  -- * string is parsed as a series of /path/to/device, write_bps elements,
  -- * separated by ','.
  --  

  -- Set Blkio tunables for the domain 
   function virDomainSetBlkioParameters
     (domain : virDomainPtr;
      params : virTypedParameterPtr;
      nparams : int;
      flags : unsigned) return int;  -- /usr/include/libvirt/libvirt-domain.h:1350
   pragma Import (C, virDomainSetBlkioParameters, "virDomainSetBlkioParameters");

   function virDomainGetBlkioParameters
     (domain : virDomainPtr;
      params : virTypedParameterPtr;
      nparams : access int;
      flags : unsigned) return int;  -- /usr/include/libvirt/libvirt-domain.h:1353
   pragma Import (C, virDomainGetBlkioParameters, "virDomainGetBlkioParameters");

  -- Manage memory parameters.   
  --*
  -- * VIR_DOMAIN_MEMORY_PARAM_UNLIMITED:
  -- *
  -- * Macro providing the virMemoryParameter value that indicates "unlimited"
  --  

  --*
  -- * VIR_DOMAIN_MEMORY_HARD_LIMIT:
  -- *
  -- * Macro for the memory tunable hard_limit: it represents the maximum memory
  -- * the guest can use, as a ullong.
  --  

  --*
  -- * VIR_DOMAIN_MEMORY_SOFT_LIMIT:
  -- *
  -- * Macro for the memory tunable soft_limit: it represents the memory upper
  -- * limit enforced during memory contention, as a ullong.
  --  

  --*
  -- * VIR_DOMAIN_MEMORY_MIN_GUARANTEE:
  -- *
  -- * Macro for the memory tunable min_guarantee: it represents the minimum
  -- * memory guaranteed to be reserved for the guest, as a ullong.
  --  

  --*
  -- * VIR_DOMAIN_MEMORY_SWAP_HARD_LIMIT:
  -- *
  -- * Macro for the swap tunable swap_hard_limit: it represents the maximum swap
  -- * plus memory the guest can use, as a ullong. This limit has to be more than
  -- * VIR_DOMAIN_MEMORY_HARD_LIMIT.
  --  

  -- Set memory tunables for the domain 
   function virDomainSetMemoryParameters
     (domain : virDomainPtr;
      params : virTypedParameterPtr;
      nparams : int;
      flags : unsigned) return int;  -- /usr/include/libvirt/libvirt-domain.h:1405
   pragma Import (C, virDomainSetMemoryParameters, "virDomainSetMemoryParameters");

   function virDomainGetMemoryParameters
     (domain : virDomainPtr;
      params : virTypedParameterPtr;
      nparams : access int;
      flags : unsigned) return int;  -- /usr/include/libvirt/libvirt-domain.h:1408
   pragma Import (C, virDomainGetMemoryParameters, "virDomainGetMemoryParameters");

  -- Memory size modification flags.  
  -- See virDomainModificationImpact for these flags.   
  -- Additionally, these flags may be bitwise-OR'd in.   
  -- affect Max rather than current  
   subtype virDomainMemoryModFlags is unsigned;
   VIR_DOMAIN_MEM_CURRENT : constant virDomainMemoryModFlags := 0;
   VIR_DOMAIN_MEM_LIVE : constant virDomainMemoryModFlags := 1;
   VIR_DOMAIN_MEM_CONFIG : constant virDomainMemoryModFlags := 2;
   VIR_DOMAIN_MEM_MAXIMUM : constant virDomainMemoryModFlags := 4;  -- /usr/include/libvirt/libvirt-domain.h:1421

  -- Manage numa parameters  
  --*
  -- * virDomainNumatuneMemMode:
  -- * Representation of the various modes in the <numatune> element of
  -- * a domain.
  --  

  -- This constant is subject to change  
   type virDomainNumatuneMemMode is 
     (VIR_DOMAIN_NUMATUNE_MEM_STRICT,
      VIR_DOMAIN_NUMATUNE_MEM_PREFERRED,
      VIR_DOMAIN_NUMATUNE_MEM_INTERLEAVE);
   pragma Convention (C, virDomainNumatuneMemMode);  -- /usr/include/libvirt/libvirt-domain.h:1439

  --*
  -- * VIR_DOMAIN_NUMA_NODESET:
  -- *
  -- * Macro for typed parameter name that lists the numa nodeset of a
  -- * domain, as a string.
  --  

  --*
  -- * VIR_DOMAIN_NUMA_MODE:
  -- *
  -- * Macro for typed parameter name that lists the numa mode of a domain,
  -- * as an int containing a virDomainNumatuneMemMode value.
  --  

   function virDomainSetNumaParameters
     (domain : virDomainPtr;
      params : virTypedParameterPtr;
      nparams : int;
      flags : unsigned) return int;  -- /usr/include/libvirt/libvirt-domain.h:1457
   pragma Import (C, virDomainSetNumaParameters, "virDomainSetNumaParameters");

   function virDomainGetNumaParameters
     (domain : virDomainPtr;
      params : virTypedParameterPtr;
      nparams : access int;
      flags : unsigned) return int;  -- /usr/include/libvirt/libvirt-domain.h:1460
   pragma Import (C, virDomainGetNumaParameters, "virDomainGetNumaParameters");

  -- * Dynamic control of domains
  --  

   function virDomainGetName (domain : virDomainPtr) return Interfaces.C.Strings.chars_ptr;  -- /usr/include/libvirt/libvirt-domain.h:1467
   pragma Import (C, virDomainGetName, "virDomainGetName");

   function virDomainGetID (domain : virDomainPtr) return unsigned;  -- /usr/include/libvirt/libvirt-domain.h:1468
   pragma Import (C, virDomainGetID, "virDomainGetID");

   function virDomainGetUUID (domain : virDomainPtr; uuid : access unsigned_char) return int;  -- /usr/include/libvirt/libvirt-domain.h:1469
   pragma Import (C, virDomainGetUUID, "virDomainGetUUID");

   function virDomainGetUUIDString (domain : virDomainPtr; buf : Interfaces.C.Strings.chars_ptr) return int;  -- /usr/include/libvirt/libvirt-domain.h:1471
   pragma Import (C, virDomainGetUUIDString, "virDomainGetUUIDString");

   function virDomainGetOSType (domain : virDomainPtr) return Interfaces.C.Strings.chars_ptr;  -- /usr/include/libvirt/libvirt-domain.h:1473
   pragma Import (C, virDomainGetOSType, "virDomainGetOSType");

   function virDomainGetMaxMemory (domain : virDomainPtr) return unsigned_long;  -- /usr/include/libvirt/libvirt-domain.h:1474
   pragma Import (C, virDomainGetMaxMemory, "virDomainGetMaxMemory");

   function virDomainSetMaxMemory (domain : virDomainPtr; memory : unsigned_long) return int;  -- /usr/include/libvirt/libvirt-domain.h:1475
   pragma Import (C, virDomainSetMaxMemory, "virDomainSetMaxMemory");

   function virDomainSetMemory (domain : virDomainPtr; memory : unsigned_long) return int;  -- /usr/include/libvirt/libvirt-domain.h:1477
   pragma Import (C, virDomainSetMemory, "virDomainSetMemory");

   function virDomainSetMemoryFlags
     (domain : virDomainPtr;
      memory : unsigned_long;
      flags : unsigned) return int;  -- /usr/include/libvirt/libvirt-domain.h:1479
   pragma Import (C, virDomainSetMemoryFlags, "virDomainSetMemoryFlags");

   function virDomainSetMemoryStatsPeriod
     (domain : virDomainPtr;
      period : int;
      flags : unsigned) return int;  -- /usr/include/libvirt/libvirt-domain.h:1482
   pragma Import (C, virDomainSetMemoryStatsPeriod, "virDomainSetMemoryStatsPeriod");

   function virDomainGetMaxVcpus (domain : virDomainPtr) return int;  -- /usr/include/libvirt/libvirt-domain.h:1485
   pragma Import (C, virDomainGetMaxVcpus, "virDomainGetMaxVcpus");

   function virDomainGetSecurityLabel (domain : virDomainPtr; seclabel : virSecurityLabelPtr) return int;  -- /usr/include/libvirt/libvirt-domain.h:1486
   pragma Import (C, virDomainGetSecurityLabel, "virDomainGetSecurityLabel");

   function virDomainGetHostname (domain : virDomainPtr; flags : unsigned) return Interfaces.C.Strings.chars_ptr;  -- /usr/include/libvirt/libvirt-domain.h:1488
   pragma Import (C, virDomainGetHostname, "virDomainGetHostname");

   function virDomainGetSecurityLabelList (domain : virDomainPtr; seclabels : System.Address) return int;  -- /usr/include/libvirt/libvirt-domain.h:1490
   pragma Import (C, virDomainGetSecurityLabelList, "virDomainGetSecurityLabelList");

  -- Operate on <description>  
  -- Operate on <title>  
  -- Operate on <metadata>  
   type virDomainMetadataType is 
     (VIR_DOMAIN_METADATA_DESCRIPTION,
      VIR_DOMAIN_METADATA_TITLE,
      VIR_DOMAIN_METADATA_ELEMENT);
   pragma Convention (C, virDomainMetadataType);  -- /usr/include/libvirt/libvirt-domain.h:1501

   function virDomainSetMetadata
     (domain : virDomainPtr;
      c_type : int;
      metadata : Interfaces.C.Strings.chars_ptr;
      key : Interfaces.C.Strings.chars_ptr;
      uri : Interfaces.C.Strings.chars_ptr;
      flags : unsigned) return int;  -- /usr/include/libvirt/libvirt-domain.h:1504
   pragma Import (C, virDomainSetMetadata, "virDomainSetMetadata");

   function virDomainGetMetadata
     (domain : virDomainPtr;
      c_type : int;
      uri : Interfaces.C.Strings.chars_ptr;
      flags : unsigned) return Interfaces.C.Strings.chars_ptr;  -- /usr/include/libvirt/libvirt-domain.h:1512
   pragma Import (C, virDomainGetMetadata, "virDomainGetMetadata");

  -- * XML domain description
  --  

  --*
  -- * virDomainXMLFlags:
  -- *
  -- * Flags available for virDomainGetXMLDesc
  --  

  -- dump security sensitive information too  
  -- dump inactive domain information  
  -- update guest CPU requirements according to host CPU  
  -- dump XML suitable for migration  
   subtype virDomainXMLFlags is unsigned;
   VIR_DOMAIN_XML_SECURE : constant virDomainXMLFlags := 1;
   VIR_DOMAIN_XML_INACTIVE : constant virDomainXMLFlags := 2;
   VIR_DOMAIN_XML_UPDATE_CPU : constant virDomainXMLFlags := 4;
   VIR_DOMAIN_XML_MIGRATABLE : constant virDomainXMLFlags := 8;  -- /usr/include/libvirt/libvirt-domain.h:1531

   function virDomainGetXMLDesc (domain : virDomainPtr; flags : unsigned) return Interfaces.C.Strings.chars_ptr;  -- /usr/include/libvirt/libvirt-domain.h:1533
   pragma Import (C, virDomainGetXMLDesc, "virDomainGetXMLDesc");

   function virConnectDomainXMLFromNative
     (conn : VirConnectPtr;
      nativeFormat : Interfaces.C.Strings.chars_ptr;
      nativeConfig : Interfaces.C.Strings.chars_ptr;
      flags : unsigned) return Interfaces.C.Strings.chars_ptr;  -- /usr/include/libvirt/libvirt-domain.h:1537
   pragma Import (C, virConnectDomainXMLFromNative, "virConnectDomainXMLFromNative");

   function virConnectDomainXMLToNative
     (conn : VirConnectPtr;
      nativeFormat : Interfaces.C.Strings.chars_ptr;
      domainXml : Interfaces.C.Strings.chars_ptr;
      flags : unsigned) return Interfaces.C.Strings.chars_ptr;  -- /usr/include/libvirt/libvirt-domain.h:1541
   pragma Import (C, virConnectDomainXMLToNative, "virConnectDomainXMLToNative");

   function virDomainBlockStats
     (dom : virDomainPtr;
      disk : Interfaces.C.Strings.chars_ptr;
      stats : virDomainBlockStatsPtr;
      size : size_t) return int;  -- /usr/include/libvirt/libvirt-domain.h:1546
   pragma Import (C, virDomainBlockStats, "virDomainBlockStats");

   function virDomainBlockStatsFlags
     (dom : virDomainPtr;
      disk : Interfaces.C.Strings.chars_ptr;
      params : virTypedParameterPtr;
      nparams : access int;
      flags : unsigned) return int;  -- /usr/include/libvirt/libvirt-domain.h:1550
   pragma Import (C, virDomainBlockStatsFlags, "virDomainBlockStatsFlags");

   function virDomainInterfaceStats
     (dom : virDomainPtr;
      path : Interfaces.C.Strings.chars_ptr;
      stats : virDomainInterfaceStatsPtr;
      size : size_t) return int;  -- /usr/include/libvirt/libvirt-domain.h:1555
   pragma Import (C, virDomainInterfaceStats, "virDomainInterfaceStats");

  -- Management of interface parameters  
  --*
  -- * VIR_DOMAIN_BANDWIDTH_IN_AVERAGE:
  -- *
  -- * Macro represents the inbound average of NIC bandwidth, as a uint.
  --  

  --*
  -- * VIR_DOMAIN_BANDWIDTH_IN_PEAK:
  -- *
  -- * Macro represents the inbound peak of NIC bandwidth, as a uint.
  --  

  --*
  -- * VIR_DOMAIN_BANDWIDTH_IN_BURST:
  -- *
  -- * Macro represents the inbound burst of NIC bandwidth, as a uint.
  --  

  --*
  -- * VIR_DOMAIN_BANDWIDTH_IN_FLOOR:
  -- *
  -- * Macro represents the inbound floor of NIC bandwidth, as a uint.
  --  

  --*
  -- * VIR_DOMAIN_BANDWIDTH_OUT_AVERAGE:
  -- *
  -- * Macro represents the outbound average of NIC bandwidth, as a uint.
  --  

  --*
  -- * VIR_DOMAIN_BANDWIDTH_OUT_PEAK:
  -- *
  -- * Macro represents the outbound peak of NIC bandwidth, as a uint.
  --  

  --*
  -- * VIR_DOMAIN_BANDWIDTH_OUT_BURST:
  -- *
  -- * Macro represents the outbound burst of NIC bandwidth, as a uint.
  --  

   function virDomainSetInterfaceParameters
     (dom : virDomainPtr;
      device : Interfaces.C.Strings.chars_ptr;
      params : virTypedParameterPtr;
      nparams : int;
      flags : unsigned) return int;  -- /usr/include/libvirt/libvirt-domain.h:1611
   pragma Import (C, virDomainSetInterfaceParameters, "virDomainSetInterfaceParameters");

   function virDomainGetInterfaceParameters
     (dom : virDomainPtr;
      device : Interfaces.C.Strings.chars_ptr;
      params : virTypedParameterPtr;
      nparams : access int;
      flags : unsigned) return int;  -- /usr/include/libvirt/libvirt-domain.h:1615
   pragma Import (C, virDomainGetInterfaceParameters, "virDomainGetInterfaceParameters");

  -- Management of domain block devices  
   function virDomainBlockPeek
     (dom : virDomainPtr;
      disk : Interfaces.C.Strings.chars_ptr;
      offset : Extensions.unsigned_long_long;
      size : size_t;
      buffer : System.Address;
      flags : unsigned) return int;  -- /usr/include/libvirt/libvirt-domain.h:1622
   pragma Import (C, virDomainBlockPeek, "virDomainBlockPeek");

  --*
  -- * virDomainBlockResizeFlags:
  -- *
  -- * Flags available for virDomainBlockResize().
  --  

  -- size in bytes instead of KiB  
   subtype virDomainBlockResizeFlags is unsigned;
   VIR_DOMAIN_BLOCK_RESIZE_BYTES : constant virDomainBlockResizeFlags := 1;  -- /usr/include/libvirt/libvirt-domain.h:1636

   function virDomainBlockResize
     (dom : virDomainPtr;
      disk : Interfaces.C.Strings.chars_ptr;
      size : Extensions.unsigned_long_long;
      flags : unsigned) return int;  -- /usr/include/libvirt/libvirt-domain.h:1638
   pragma Import (C, virDomainBlockResize, "virDomainBlockResize");

  --* virDomainBlockInfo:
  -- *
  -- * This struct provides information about the size of a block device
  -- * backing store.
  -- *
  -- * Examples:
  -- *
  -- *  - Fully allocated raw file in filesystem:
  -- *       * capacity, allocation, physical: All the same
  -- *
  -- *  - Sparse raw file in filesystem:
  -- *       * capacity, size: logical size of the file
  -- *       * allocation: disk space occupied by file
  -- *
  -- *  - qcow2 file in filesystem
  -- *       * capacity: logical size from qcow2 header
  -- *       * allocation: disk space occupied by file
  -- *       * physical: reported size of qcow2 file
  -- *
  -- *  - qcow2 file in a block device
  -- *       * capacity: logical size from qcow2 header
  -- *       * allocation: highest qcow extent written for an active domain
  -- *       * physical: size of the block device container
  --  

   type u_virDomainBlockInfo;
   subtype virDomainBlockInfo is u_virDomainBlockInfo;

   type virDomainBlockInfoPtr is new System.Address;  -- /usr/include/libvirt/libvirt-domain.h:1668

  -- logical size in bytes of the
  --                                    * image (how much storage the
  --                                    * guest will see)  

   type u_virDomainBlockInfo is record
      capacity : aliased Extensions.unsigned_long_long;  -- /usr/include/libvirt/libvirt-domain.h:1670
      allocation : aliased Extensions.unsigned_long_long;  -- /usr/include/libvirt/libvirt-domain.h:1673
      physical : aliased Extensions.unsigned_long_long;  -- /usr/include/libvirt/libvirt-domain.h:1677
   end record;
   pragma Convention (C_Pass_By_Copy, u_virDomainBlockInfo);  -- /usr/include/libvirt/libvirt-domain.h:1669

  -- host storage in bytes occupied
  --                                    * by the image (such as highest
  --                                    * allocated extent if there are no
  --                                    * holes, similar to 'du')  

  -- host physical size in bytes of
  --                                    * the image container (last
  --                                    * offset, similar to 'ls') 

   function virDomainGetBlockInfo
     (dom : virDomainPtr;
      disk : Interfaces.C.Strings.chars_ptr;
      info : virDomainBlockInfoPtr;
      flags : unsigned) return int;  -- /usr/include/libvirt/libvirt-domain.h:1682
   pragma Import (C, virDomainGetBlockInfo, "virDomainGetBlockInfo");

  -- Management of domain memory  
   function virDomainMemoryStats
     (dom : virDomainPtr;
      stats : virDomainMemoryStatPtr;
      nr_stats : unsigned;
      flags : unsigned) return int;  -- /usr/include/libvirt/libvirt-domain.h:1689
   pragma Import (C, virDomainMemoryStats, "virDomainMemoryStats");

  -- Memory peeking flags.  
  -- addresses are virtual addresses  
  -- addresses are physical addresses  
   subtype virDomainMemoryFlags is unsigned;
   VIR_MEMORY_VIRTUAL : constant virDomainMemoryFlags := 1;
   VIR_MEMORY_PHYSICAL : constant virDomainMemoryFlags := 2;  -- /usr/include/libvirt/libvirt-domain.h:1699

   function virDomainMemoryPeek
     (dom : virDomainPtr;
      start : Extensions.unsigned_long_long;
      size : size_t;
      buffer : System.Address;
      flags : unsigned) return int;  -- /usr/include/libvirt/libvirt-domain.h:1701
   pragma Import (C, virDomainMemoryPeek, "virDomainMemoryPeek");

  -- Validate the XML document against schema  
   subtype virDomainDefineFlags is unsigned;
   VIR_DOMAIN_DEFINE_VALIDATE : constant virDomainDefineFlags := 1;  -- /usr/include/libvirt/libvirt-domain.h:1709

  -- * defined but not running domains
  --  

   function virDomainDefineXML (conn : VirConnectPtr; xml : Interfaces.C.Strings.chars_ptr) return virDomainPtr;  -- /usr/include/libvirt/libvirt-domain.h:1714
   pragma Import (C, virDomainDefineXML, "virDomainDefineXML");

   function virDomainDefineXMLFlags
     (conn : VirConnectPtr;
      xml : Interfaces.C.Strings.chars_ptr;
      flags : unsigned) return virDomainPtr;  -- /usr/include/libvirt/libvirt-domain.h:1717
   pragma Import (C, virDomainDefineXMLFlags, "virDomainDefineXMLFlags");

   function virDomainUndefine (domain : virDomainPtr) return int;  -- /usr/include/libvirt/libvirt-domain.h:1720
   pragma Import (C, virDomainUndefine, "virDomainUndefine");

  -- Also remove any
  --                                                          managed save  

  -- If last use of domain,
  --                                                          then also remove any
  --                                                          snapshot metadata  

  -- Also remove any
  --                                                          nvram file  

  -- Keep nvram file  
  -- Future undefine control flags should come here.  
   subtype virDomainUndefineFlagsValues is unsigned;
   VIR_DOMAIN_UNDEFINE_MANAGED_SAVE : constant virDomainUndefineFlagsValues := 1;
   VIR_DOMAIN_UNDEFINE_SNAPSHOTS_METADATA : constant virDomainUndefineFlagsValues := 2;
   VIR_DOMAIN_UNDEFINE_NVRAM : constant virDomainUndefineFlagsValues := 4;
   VIR_DOMAIN_UNDEFINE_KEEP_NVRAM : constant virDomainUndefineFlagsValues := 8;  -- /usr/include/libvirt/libvirt-domain.h:1733

   function virDomainUndefineFlags (domain : virDomainPtr; flags : unsigned) return int;  -- /usr/include/libvirt/libvirt-domain.h:1736
   pragma Import (C, virDomainUndefineFlags, "virDomainUndefineFlags");

   function virConnectNumOfDefinedDomains (conn : VirConnectPtr) return int;  -- /usr/include/libvirt/libvirt-domain.h:1738
   pragma Import (C, virConnectNumOfDefinedDomains, "virConnectNumOfDefinedDomains");

   function virConnectListDefinedDomains
     (conn : VirConnectPtr;
      names : System.Address;
      maxnames : int) return int;  -- /usr/include/libvirt/libvirt-domain.h:1739
   pragma Import (C, virConnectListDefinedDomains, "virConnectListDefinedDomains");

  --*
  -- * virConnectListAllDomainsFlags:
  -- *
  -- * Flags used to tune which domains are listed by virConnectListAllDomains().
  -- * Note that these flags come in groups; if all bits from a group are 0,
  -- * then that group is not used to filter results.
  --  

   subtype virConnectListAllDomainsFlags is unsigned;
   VIR_CONNECT_LIST_DOMAINS_ACTIVE : constant virConnectListAllDomainsFlags := 1;
   VIR_CONNECT_LIST_DOMAINS_INACTIVE : constant virConnectListAllDomainsFlags := 2;
   VIR_CONNECT_LIST_DOMAINS_PERSISTENT : constant virConnectListAllDomainsFlags := 4;
   VIR_CONNECT_LIST_DOMAINS_TRANSIENT : constant virConnectListAllDomainsFlags := 8;
   VIR_CONNECT_LIST_DOMAINS_RUNNING : constant virConnectListAllDomainsFlags := 16;
   VIR_CONNECT_LIST_DOMAINS_PAUSED : constant virConnectListAllDomainsFlags := 32;
   VIR_CONNECT_LIST_DOMAINS_SHUTOFF : constant virConnectListAllDomainsFlags := 64;
   VIR_CONNECT_LIST_DOMAINS_OTHER : constant virConnectListAllDomainsFlags := 128;
   VIR_CONNECT_LIST_DOMAINS_MANAGEDSAVE : constant virConnectListAllDomainsFlags := 256;
   VIR_CONNECT_LIST_DOMAINS_NO_MANAGEDSAVE : constant virConnectListAllDomainsFlags := 512;
   VIR_CONNECT_LIST_DOMAINS_AUTOSTART : constant virConnectListAllDomainsFlags := 1024;
   VIR_CONNECT_LIST_DOMAINS_NO_AUTOSTART : constant virConnectListAllDomainsFlags := 2048;
   VIR_CONNECT_LIST_DOMAINS_HAS_SNAPSHOT : constant virConnectListAllDomainsFlags := 4096;
   VIR_CONNECT_LIST_DOMAINS_NO_SNAPSHOT : constant virConnectListAllDomainsFlags := 8192;  -- /usr/include/libvirt/libvirt-domain.h:1769

   function virConnectListAllDomains
     (conn : VirConnectPtr;
      domains : System.Address;
      flags : unsigned) return int;  -- /usr/include/libvirt/libvirt-domain.h:1771
   pragma Import (C, virConnectListAllDomains, "virConnectListAllDomains");

   function virDomainCreate (domain : virDomainPtr) return int;  -- /usr/include/libvirt/libvirt-domain.h:1774
   pragma Import (C, virDomainCreate, "virDomainCreate");

   function virDomainCreateWithFlags (domain : virDomainPtr; flags : unsigned) return int;  -- /usr/include/libvirt/libvirt-domain.h:1775
   pragma Import (C, virDomainCreateWithFlags, "virDomainCreateWithFlags");

   function virDomainCreateWithFiles
     (domain : virDomainPtr;
      nfiles : unsigned;
      files : access int;
      flags : unsigned) return int;  -- /usr/include/libvirt/libvirt-domain.h:1778
   pragma Import (C, virDomainCreateWithFiles, "virDomainCreateWithFiles");

   function virDomainGetAutostart (domain : virDomainPtr; autostart : access int) return int;  -- /usr/include/libvirt/libvirt-domain.h:1783
   pragma Import (C, virDomainGetAutostart, "virDomainGetAutostart");

   function virDomainSetAutostart (domain : virDomainPtr; autostart : int) return int;  -- /usr/include/libvirt/libvirt-domain.h:1785
   pragma Import (C, virDomainSetAutostart, "virDomainSetAutostart");

  --*
  -- * virVcpuInfo: structure for information about a virtual CPU in a domain.
  --  

  -- the virtual CPU is offline  
  -- the virtual CPU is running  
  -- the virtual CPU is blocked on resource  
   type virVcpuState is 
     (VIR_VCPU_OFFLINE,
      VIR_VCPU_RUNNING,
      VIR_VCPU_BLOCKED);
   pragma Convention (C, virVcpuState);  -- /usr/include/libvirt/libvirt-domain.h:1800

   type u_virVcpuInfo;
   subtype virVcpuInfo is u_virVcpuInfo;

  -- virtual CPU number  
   type u_virVcpuInfo is record
      number : aliased unsigned;  -- /usr/include/libvirt/libvirt-domain.h:1804
      state : aliased int;  -- /usr/include/libvirt/libvirt-domain.h:1805
      cpuTime : aliased Extensions.unsigned_long_long;  -- /usr/include/libvirt/libvirt-domain.h:1806
      cpu : aliased int;  -- /usr/include/libvirt/libvirt-domain.h:1807
   end record;
   pragma Convention (C_Pass_By_Copy, u_virVcpuInfo);  -- /usr/include/libvirt/libvirt-domain.h:1803

  -- value from virVcpuState  
  -- CPU time used, in nanoseconds  
  -- real CPU number, or -1 if offline  
   type virVcpuInfoPtr is access all virVcpuInfo;  -- /usr/include/libvirt/libvirt-domain.h:1809

  -- Flags for controlling virtual CPU hot-plugging.   
  -- See virDomainModificationImpact for these flags.   
  -- Additionally, these flags may be bitwise-OR'd in.   
  -- Max rather than current count  
  -- Modify state of the cpu in the guest  
  -- Make vcpus added hot(un)pluggable  
   subtype virDomainVcpuFlags is unsigned;
   VIR_DOMAIN_VCPU_CURRENT : constant virDomainVcpuFlags := 0;
   VIR_DOMAIN_VCPU_LIVE : constant virDomainVcpuFlags := 1;
   VIR_DOMAIN_VCPU_CONFIG : constant virDomainVcpuFlags := 2;
   VIR_DOMAIN_VCPU_MAXIMUM : constant virDomainVcpuFlags := 4;
   VIR_DOMAIN_VCPU_GUEST : constant virDomainVcpuFlags := 8;
   VIR_DOMAIN_VCPU_HOTPLUGGABLE : constant virDomainVcpuFlags := 16;  -- /usr/include/libvirt/libvirt-domain.h:1822

   function virDomainSetVcpus (domain : virDomainPtr; nvcpus : unsigned) return int;  -- /usr/include/libvirt/libvirt-domain.h:1824
   pragma Import (C, virDomainSetVcpus, "virDomainSetVcpus");

   function virDomainSetVcpusFlags
     (domain : virDomainPtr;
      nvcpus : unsigned;
      flags : unsigned) return int;  -- /usr/include/libvirt/libvirt-domain.h:1826
   pragma Import (C, virDomainSetVcpusFlags, "virDomainSetVcpusFlags");

   function virDomainGetVcpusFlags (domain : virDomainPtr; flags : unsigned) return int;  -- /usr/include/libvirt/libvirt-domain.h:1829
   pragma Import (C, virDomainGetVcpusFlags, "virDomainGetVcpusFlags");

   function virDomainPinVcpu
     (domain : virDomainPtr;
      vcpu : unsigned;
      cpumap : access unsigned_char;
      maplen : int) return int;  -- /usr/include/libvirt/libvirt-domain.h:1832
   pragma Import (C, virDomainPinVcpu, "virDomainPinVcpu");

   function virDomainPinVcpuFlags
     (domain : virDomainPtr;
      vcpu : unsigned;
      cpumap : access unsigned_char;
      maplen : int;
      flags : unsigned) return int;  -- /usr/include/libvirt/libvirt-domain.h:1836
   pragma Import (C, virDomainPinVcpuFlags, "virDomainPinVcpuFlags");

   function virDomainGetVcpuPinInfo
     (domain : virDomainPtr;
      ncpumaps : int;
      cpumaps : access unsigned_char;
      maplen : int;
      flags : unsigned) return int;  -- /usr/include/libvirt/libvirt-domain.h:1842
   pragma Import (C, virDomainGetVcpuPinInfo, "virDomainGetVcpuPinInfo");

   function virDomainPinEmulator
     (domain : virDomainPtr;
      cpumap : access unsigned_char;
      maplen : int;
      flags : unsigned) return int;  -- /usr/include/libvirt/libvirt-domain.h:1848
   pragma Import (C, virDomainPinEmulator, "virDomainPinEmulator");

   function virDomainGetEmulatorPinInfo
     (domain : virDomainPtr;
      cpumaps : access unsigned_char;
      maplen : int;
      flags : unsigned) return int;  -- /usr/include/libvirt/libvirt-domain.h:1853
   pragma Import (C, virDomainGetEmulatorPinInfo, "virDomainGetEmulatorPinInfo");

  --*
  -- * virIOThreadInfo:
  -- *
  -- * The data structure for information about all IOThreads in a domain
  --  

   type u_virDomainIOThreadInfo;
   subtype virDomainIOThreadInfo is u_virDomainIOThreadInfo;

   type virDomainIOThreadInfoPtr is new System.Address;  -- /usr/include/libvirt/libvirt-domain.h:1864

  -- IOThread ID  
   type u_virDomainIOThreadInfo is record
      iothread_id : aliased unsigned;  -- /usr/include/libvirt/libvirt-domain.h:1866
      cpumap : access unsigned_char;  -- /usr/include/libvirt/libvirt-domain.h:1867
      cpumaplen : aliased int;  -- /usr/include/libvirt/libvirt-domain.h:1869
   end record;
   pragma Convention (C_Pass_By_Copy, u_virDomainIOThreadInfo);  -- /usr/include/libvirt/libvirt-domain.h:1865

  -- CPU map for thread. A pointer to an  
  -- array of real CPUs (in 8-bit bytes)  
  -- cpumap size  
   procedure virDomainIOThreadInfoFree (info : virDomainIOThreadInfoPtr);  -- /usr/include/libvirt/libvirt-domain.h:1872
   pragma Import (C, virDomainIOThreadInfoFree, "virDomainIOThreadInfoFree");

   function virDomainGetIOThreadInfo
     (domain : virDomainPtr;
      info : System.Address;
      flags : unsigned) return int;  -- /usr/include/libvirt/libvirt-domain.h:1874
   pragma Import (C, virDomainGetIOThreadInfo, "virDomainGetIOThreadInfo");

   function virDomainPinIOThread
     (domain : virDomainPtr;
      iothread_id : unsigned;
      cpumap : access unsigned_char;
      maplen : int;
      flags : unsigned) return int;  -- /usr/include/libvirt/libvirt-domain.h:1877
   pragma Import (C, virDomainPinIOThread, "virDomainPinIOThread");

   function virDomainAddIOThread
     (domain : virDomainPtr;
      iothread_id : unsigned;
      flags : unsigned) return int;  -- /usr/include/libvirt/libvirt-domain.h:1882
   pragma Import (C, virDomainAddIOThread, "virDomainAddIOThread");

   function virDomainDelIOThread
     (domain : virDomainPtr;
      iothread_id : unsigned;
      flags : unsigned) return int;  -- /usr/include/libvirt/libvirt-domain.h:1885
   pragma Import (C, virDomainDelIOThread, "virDomainDelIOThread");

  --*
  -- * VIR_USE_CPU:
  -- * @cpumap: pointer to a bit map of real CPUs (in 8-bit bytes) (IN/OUT)
  -- * @cpu: the physical CPU number
  -- *
  -- * This macro is to be used in conjunction with virDomainPinVcpu() API.
  -- * It sets the bit (CPU usable) of the related cpu in cpumap.
  --  

  --*
  -- * VIR_UNUSE_CPU:
  -- * @cpumap: pointer to a bit map of real CPUs (in 8-bit bytes) (IN/OUT)
  -- * @cpu: the physical CPU number
  -- *
  -- * This macro is to be used in conjunction with virDomainPinVcpu() API.
  -- * It resets the bit (CPU not usable) of the related cpu in cpumap.
  --  

  --*
  -- * VIR_CPU_USED:
  -- * @cpumap: pointer to a bit map of real CPUs (in 8-bit bytes) (IN)
  -- * @cpu: the physical CPU number
  -- *
  -- * This macro can be used in conjunction with virNodeGetCPUMap() API.
  -- * It returns non-zero if the bit of the related CPU is set.
  --  

  --*
  -- * VIR_CPU_MAPLEN:
  -- * @cpu: number of physical CPUs
  -- *
  -- * This macro is to be used in conjunction with virDomainPinVcpu() API.
  -- * It returns the length (in bytes) required to store the complete
  -- * CPU map between a single virtual & all physical CPUs of a domain.
  --  

   function virDomainGetVcpus
     (domain : virDomainPtr;
      info : virVcpuInfoPtr;
      maxinfo : int;
      cpumaps : access unsigned_char;
      maplen : int) return int;  -- /usr/include/libvirt/libvirt-domain.h:1934
   pragma Import (C, virDomainGetVcpus, "virDomainGetVcpus");

  --*
  -- * VIR_CPU_USABLE:
  -- * @cpumaps: pointer to an array of cpumap (in 8-bit bytes) (IN)
  -- * @maplen: the length (in bytes) of one cpumap
  -- * @vcpu: the virtual CPU number
  -- * @cpu: the physical CPU number
  -- *
  -- * This macro is to be used in conjunction with virDomainGetVcpus() API.
  -- * VIR_CPU_USABLE macro returns a non-zero value (true) if the cpu
  -- * is usable by the vcpu, and 0 otherwise.
  --  

  --*
  -- * VIR_COPY_CPUMAP:
  -- * @cpumaps: pointer to an array of cpumap (in 8-bit bytes) (IN)
  -- * @maplen: the length (in bytes) of one cpumap
  -- * @vcpu: the virtual CPU number
  -- * @cpumap: pointer to a cpumap (in 8-bit bytes) (OUT)
  -- *      This cpumap must be previously allocated by the caller
  -- *      (ie: malloc(maplen))
  -- *
  -- * This macro is to be used in conjunction with virDomainGetVcpus() and
  -- * virDomainPinVcpu() APIs. VIR_COPY_CPUMAP macro extracts the cpumap of
  -- * the specified vcpu from cpumaps array and copies it into cpumap to be used
  -- * later by virDomainPinVcpu() API.
  --  

  --*
  -- * VIR_GET_CPUMAP:
  -- * @cpumaps: pointer to an array of cpumap (in 8-bit bytes) (IN)
  -- * @maplen: the length (in bytes) of one cpumap
  -- * @vcpu: the virtual CPU number
  -- *
  -- * This macro is to be used in conjunction with virDomainGetVcpus() and
  -- * virDomainPinVcpu() APIs. VIR_GET_CPUMAP macro returns a pointer to the
  -- * cpumap of the specified vcpu from cpumaps array.
  --  

  -- See virDomainModificationImpact for these flags.   
  -- Additionally, these flags may be bitwise-OR'd in.   
  -- Forcibly modify device
  --                                                  (ex. force eject a cdrom)  

   subtype virDomainDeviceModifyFlags is unsigned;
   VIR_DOMAIN_DEVICE_MODIFY_CURRENT : constant virDomainDeviceModifyFlags := 0;
   VIR_DOMAIN_DEVICE_MODIFY_LIVE : constant virDomainDeviceModifyFlags := 1;
   VIR_DOMAIN_DEVICE_MODIFY_CONFIG : constant virDomainDeviceModifyFlags := 2;
   VIR_DOMAIN_DEVICE_MODIFY_FORCE : constant virDomainDeviceModifyFlags := 4;  -- /usr/include/libvirt/libvirt-domain.h:1995

   function virDomainAttachDevice (domain : virDomainPtr; xml : Interfaces.C.Strings.chars_ptr) return int;  -- /usr/include/libvirt/libvirt-domain.h:1997
   pragma Import (C, virDomainAttachDevice, "virDomainAttachDevice");

   function virDomainDetachDevice (domain : virDomainPtr; xml : Interfaces.C.Strings.chars_ptr) return int;  -- /usr/include/libvirt/libvirt-domain.h:1998
   pragma Import (C, virDomainDetachDevice, "virDomainDetachDevice");

   function virDomainAttachDeviceFlags
     (domain : virDomainPtr;
      xml : Interfaces.C.Strings.chars_ptr;
      flags : unsigned) return int;  -- /usr/include/libvirt/libvirt-domain.h:2000
   pragma Import (C, virDomainAttachDeviceFlags, "virDomainAttachDeviceFlags");

   function virDomainDetachDeviceFlags
     (domain : virDomainPtr;
      xml : Interfaces.C.Strings.chars_ptr;
      flags : unsigned) return int;  -- /usr/include/libvirt/libvirt-domain.h:2002
   pragma Import (C, virDomainDetachDeviceFlags, "virDomainDetachDeviceFlags");

   function virDomainUpdateDeviceFlags
     (domain : virDomainPtr;
      xml : Interfaces.C.Strings.chars_ptr;
      flags : unsigned) return int;  -- /usr/include/libvirt/libvirt-domain.h:2004
   pragma Import (C, virDomainUpdateDeviceFlags, "virDomainUpdateDeviceFlags");

   type u_virDomainStatsRecord;
   subtype virDomainStatsRecord is u_virDomainStatsRecord;

   type virDomainStatsRecordPtr is new System.Address;  -- /usr/include/libvirt/libvirt-domain.h:2008

   type u_virDomainStatsRecord is record
      dom : virDomainPtr;  -- /usr/include/libvirt/libvirt-domain.h:2010
      params : virTypedParameterPtr;  -- /usr/include/libvirt/libvirt-domain.h:2011
      nparams : aliased int;  -- /usr/include/libvirt/libvirt-domain.h:2012
   end record;
   pragma Convention (C_Pass_By_Copy, u_virDomainStatsRecord);  -- /usr/include/libvirt/libvirt-domain.h:2009

  -- return domain state  
  -- return domain CPU info  
  -- return domain balloon info  
  -- return domain virtual CPU info  
  -- return domain interfaces info  
  -- return domain block info  
  -- return domain perf event info  
   subtype virDomainStatsTypes is unsigned;
   VIR_DOMAIN_STATS_STATE : constant virDomainStatsTypes := 1;
   VIR_DOMAIN_STATS_CPU_TOTAL : constant virDomainStatsTypes := 2;
   VIR_DOMAIN_STATS_BALLOON : constant virDomainStatsTypes := 4;
   VIR_DOMAIN_STATS_VCPU : constant virDomainStatsTypes := 8;
   VIR_DOMAIN_STATS_INTERFACE : constant virDomainStatsTypes := 16;
   VIR_DOMAIN_STATS_BLOCK : constant virDomainStatsTypes := 32;
   VIR_DOMAIN_STATS_PERF : constant virDomainStatsTypes := 64;  -- /usr/include/libvirt/libvirt-domain.h:2023

  -- include backing chain for block stats  
  -- enforce requested stats  
   subtype virConnectGetAllDomainStatsFlags is unsigned;
   VIR_CONNECT_GET_ALL_DOMAINS_STATS_ACTIVE : constant virConnectGetAllDomainStatsFlags := 1;
   VIR_CONNECT_GET_ALL_DOMAINS_STATS_INACTIVE : constant virConnectGetAllDomainStatsFlags := 2;
   VIR_CONNECT_GET_ALL_DOMAINS_STATS_PERSISTENT : constant virConnectGetAllDomainStatsFlags := 4;
   VIR_CONNECT_GET_ALL_DOMAINS_STATS_TRANSIENT : constant virConnectGetAllDomainStatsFlags := 8;
   VIR_CONNECT_GET_ALL_DOMAINS_STATS_RUNNING : constant virConnectGetAllDomainStatsFlags := 16;
   VIR_CONNECT_GET_ALL_DOMAINS_STATS_PAUSED : constant virConnectGetAllDomainStatsFlags := 32;
   VIR_CONNECT_GET_ALL_DOMAINS_STATS_SHUTOFF : constant virConnectGetAllDomainStatsFlags := 64;
   VIR_CONNECT_GET_ALL_DOMAINS_STATS_OTHER : constant virConnectGetAllDomainStatsFlags := 128;
   VIR_CONNECT_GET_ALL_DOMAINS_STATS_BACKING : constant virConnectGetAllDomainStatsFlags := 1073741824;
   VIR_CONNECT_GET_ALL_DOMAINS_STATS_ENFORCE_STATS : constant virConnectGetAllDomainStatsFlags := 2147483648;  -- /usr/include/libvirt/libvirt-domain.h:2039

   function virConnectGetAllDomainStats
     (conn : VirConnectPtr;
      stats : unsigned;
      retStats : System.Address;
      flags : unsigned) return int;  -- /usr/include/libvirt/libvirt-domain.h:2041
   pragma Import (C, virConnectGetAllDomainStats, "virConnectGetAllDomainStats");

   function virDomainListGetStats
     (doms : System.Address;
      stats : unsigned;
      retStats : System.Address;
      flags : unsigned) return int;  -- /usr/include/libvirt/libvirt-domain.h:2046
   pragma Import (C, virDomainListGetStats, "virDomainListGetStats");

   procedure virDomainStatsRecordListFree (stats : System.Address);  -- /usr/include/libvirt/libvirt-domain.h:2051
   pragma Import (C, virDomainStatsRecordListFree, "virDomainStatsRecordListFree");

  -- * Perf Event API
  --  

  --*
  -- * VIR_PERF_PARAM_CMT:
  -- *
  -- * Macro for typed parameter name that represents CMT perf event
  -- * which can be used to measure the usage of cache (bytes) by
  -- * applications running on the platform. It corresponds to the
  -- * "perf.cmt" field in the *Stats APIs.
  --  

  --*
  -- * VIR_PERF_PARAM_MBMT:
  -- *
  -- * Macro for typed parameter name that represents MBMT perf event
  -- * which can be used to monitor total system bandwidth (bytes/s)
  -- * from one level of cache to another. It corresponds to the
  -- * "perf.mbmt" field in the *Stats APIs.
  --  

  --*
  -- * VIR_PERF_PARAM_MBML:
  -- *
  -- * Macro for typed parameter name that represents MBML perf event
  -- * which can be used to monitor the amount of data (bytes/s) sent
  -- * through the memory controller on the socket. It corresponds to
  -- * the "perf.mbml" field in the *Stats APIs.
  --  

  --*
  -- * VIR_PERF_PARAM_CACHE_MISSES:
  -- *
  -- * Macro for typed parameter name that represents cache_misses perf
  -- * event which can be used to measure the count of cache misses by
  -- * applications running on the platform. It corresponds to the
  -- * "perf.cache_misses" field in the *Stats APIs.
  --  

  --*
  -- * VIR_PERF_PARAM_CACHE_REFERENCES:
  -- *
  -- * Macro for typed parameter name that represents cache_references
  -- * perf event which can be used to measure the count of cache hits
  -- * by applications running on the platform. It corresponds to the
  -- * "perf.cache_references" field in the *Stats APIs.
  --  

  --*
  -- * VIR_PERF_PARAM_INSTRUCTIONS:
  -- *
  -- * Macro for typed parameter name that represents instructions perf
  -- * event which can be used to measure the count of instructions
  -- * by applications running on the platform. It corresponds to the
  -- * "perf.instructions" field in the *Stats APIs.
  --  

  --*
  -- * VIR_PERF_PARAM_CPU_CYCLES:
  -- *
  -- * Macro for typed parameter name that represents cpu_cycles perf event
  -- * describing the total/elapsed cpu cycles. This can be used to measure
  -- * how many cpu cycles one instruction needs.
  -- * It corresponds to the "perf.cpu_cycles" field in the *Stats APIs.
  --  

   function virDomainGetPerfEvents
     (dom : virDomainPtr;
      params : System.Address;
      nparams : access int;
      flags : unsigned) return int;  -- /usr/include/libvirt/libvirt-domain.h:2128
   pragma Import (C, virDomainGetPerfEvents, "virDomainGetPerfEvents");

   function virDomainSetPerfEvents
     (dom : virDomainPtr;
      params : virTypedParameterPtr;
      nparams : int;
      flags : unsigned) return int;  -- /usr/include/libvirt/libvirt-domain.h:2132
   pragma Import (C, virDomainSetPerfEvents, "virDomainSetPerfEvents");

  -- * BlockJob API
  --  

  --*
  -- * virDomainBlockJobType:
  -- *
  -- * Describes various possible block jobs.
  --  

  -- Placeholder  
  -- Block Pull (virDomainBlockPull, or virDomainBlockRebase without
  --     * flags), job ends on completion  

  -- Block Copy (virDomainBlockCopy, or virDomainBlockRebase with
  --     * flags), job exists as long as mirroring is active  

  -- Block Commit (virDomainBlockCommit without flags), job ends on
  --     * completion  

  -- Active Block Commit (virDomainBlockCommit with flags), job
  --     * exists as long as sync is active  

   type virDomainBlockJobType is 
     (VIR_DOMAIN_BLOCK_JOB_TYPE_UNKNOWN,
      VIR_DOMAIN_BLOCK_JOB_TYPE_PULL,
      VIR_DOMAIN_BLOCK_JOB_TYPE_COPY,
      VIR_DOMAIN_BLOCK_JOB_TYPE_COMMIT,
      VIR_DOMAIN_BLOCK_JOB_TYPE_ACTIVE_COMMIT);
   pragma Convention (C, virDomainBlockJobType);  -- /usr/include/libvirt/libvirt-domain.h:2168

  --*
  -- * virDomainBlockJobAbortFlags:
  -- *
  -- * VIR_DOMAIN_BLOCK_JOB_ABORT_ASYNC: Request only, do not wait for completion
  -- * VIR_DOMAIN_BLOCK_JOB_ABORT_PIVOT: Pivot to new file when ending a copy or
  -- *                                   active commit job
  --  

   subtype virDomainBlockJobAbortFlags is unsigned;
   VIR_DOMAIN_BLOCK_JOB_ABORT_ASYNC : constant virDomainBlockJobAbortFlags := 1;
   VIR_DOMAIN_BLOCK_JOB_ABORT_PIVOT : constant virDomainBlockJobAbortFlags := 2;  -- /usr/include/libvirt/libvirt-domain.h:2180

   function virDomainBlockJobAbort
     (dom : virDomainPtr;
      disk : Interfaces.C.Strings.chars_ptr;
      flags : unsigned) return int;  -- /usr/include/libvirt/libvirt-domain.h:2182
   pragma Import (C, virDomainBlockJobAbort, "virDomainBlockJobAbort");

  -- Flags for use with virDomainGetBlockJobInfo  
  -- bandwidth in bytes/s
  --                                                           instead of MiB/s  

   subtype virDomainBlockJobInfoFlags is unsigned;
   VIR_DOMAIN_BLOCK_JOB_INFO_BANDWIDTH_BYTES : constant virDomainBlockJobInfoFlags := 1;  -- /usr/include/libvirt/libvirt-domain.h:2189

  -- An iterator for monitoring block job operations  
   subtype virDomainBlockJobCursor is Extensions.unsigned_long_long;  -- /usr/include/libvirt/libvirt-domain.h:2192

   type u_virDomainBlockJobInfo;
   subtype virDomainBlockJobInfo is u_virDomainBlockJobInfo;

  -- virDomainBlockJobType  
   type u_virDomainBlockJobInfo is record
      c_type : aliased int;  -- /usr/include/libvirt/libvirt-domain.h:2196
      bandwidth : aliased unsigned_long;  -- /usr/include/libvirt/libvirt-domain.h:2197
      cur : aliased virDomainBlockJobCursor;  -- /usr/include/libvirt/libvirt-domain.h:2205
      c_end : aliased virDomainBlockJobCursor;  -- /usr/include/libvirt/libvirt-domain.h:2206
   end record;
   pragma Convention (C_Pass_By_Copy, u_virDomainBlockJobInfo);  -- /usr/include/libvirt/libvirt-domain.h:2195

  -- either bytes/s or MiB/s, according to flags  
  --     * The following fields provide an indication of block job progress.  @cur
  --     * indicates the current position and will be between 0 and @end.  @end is
  --     * the final cursor position for this operation and represents completion.
  --     * To approximate progress, divide @cur by @end.
  --      

   type virDomainBlockJobInfoPtr is access all virDomainBlockJobInfo;  -- /usr/include/libvirt/libvirt-domain.h:2208

   function virDomainGetBlockJobInfo
     (dom : virDomainPtr;
      disk : Interfaces.C.Strings.chars_ptr;
      info : virDomainBlockJobInfoPtr;
      flags : unsigned) return int;  -- /usr/include/libvirt/libvirt-domain.h:2210
   pragma Import (C, virDomainGetBlockJobInfo, "virDomainGetBlockJobInfo");

  -- Flags for use with virDomainBlockJobSetSpeed  
  -- bandwidth in bytes/s
  --                                                            instead of MiB/s  

   subtype virDomainBlockJobSetSpeedFlags is unsigned;
   VIR_DOMAIN_BLOCK_JOB_SPEED_BANDWIDTH_BYTES : constant virDomainBlockJobSetSpeedFlags := 1;  -- /usr/include/libvirt/libvirt-domain.h:2218

   function virDomainBlockJobSetSpeed
     (dom : virDomainPtr;
      disk : Interfaces.C.Strings.chars_ptr;
      bandwidth : unsigned_long;
      flags : unsigned) return int;  -- /usr/include/libvirt/libvirt-domain.h:2220
   pragma Import (C, virDomainBlockJobSetSpeed, "virDomainBlockJobSetSpeed");

  -- Flags for use with virDomainBlockPull (values chosen to be a subset
  -- * of the flags for virDomainBlockRebase)  

  -- bandwidth in bytes/s
  --                                                       instead of MiB/s  

   subtype virDomainBlockPullFlags is unsigned;
   VIR_DOMAIN_BLOCK_PULL_BANDWIDTH_BYTES : constant virDomainBlockPullFlags := 64;  -- /usr/include/libvirt/libvirt-domain.h:2228

   function virDomainBlockPull
     (dom : virDomainPtr;
      disk : Interfaces.C.Strings.chars_ptr;
      bandwidth : unsigned_long;
      flags : unsigned) return int;  -- /usr/include/libvirt/libvirt-domain.h:2230
   pragma Import (C, virDomainBlockPull, "virDomainBlockPull");

  --*
  -- * virDomainBlockRebaseFlags:
  -- *
  -- * Flags available for virDomainBlockRebase().
  --  

  -- Limit copy to top of source
  --                                                   backing chain  

  -- Reuse existing external
  --                                                   file for a copy  

  -- Make destination file raw  
  -- Start a copy job  
  -- Keep backing chain
  --                                                   referenced using relative
  --                                                   names  

  -- Treat destination as block
  --                                                   device instead of file  

  -- bandwidth in bytes/s
  --                                                         instead of MiB/s  

   subtype virDomainBlockRebaseFlags is unsigned;
   VIR_DOMAIN_BLOCK_REBASE_SHALLOW : constant virDomainBlockRebaseFlags := 1;
   VIR_DOMAIN_BLOCK_REBASE_REUSE_EXT : constant virDomainBlockRebaseFlags := 2;
   VIR_DOMAIN_BLOCK_REBASE_COPY_RAW : constant virDomainBlockRebaseFlags := 4;
   VIR_DOMAIN_BLOCK_REBASE_COPY : constant virDomainBlockRebaseFlags := 8;
   VIR_DOMAIN_BLOCK_REBASE_RELATIVE : constant virDomainBlockRebaseFlags := 16;
   VIR_DOMAIN_BLOCK_REBASE_COPY_DEV : constant virDomainBlockRebaseFlags := 32;
   VIR_DOMAIN_BLOCK_REBASE_BANDWIDTH_BYTES : constant virDomainBlockRebaseFlags := 64;  -- /usr/include/libvirt/libvirt-domain.h:2252

   function virDomainBlockRebase
     (dom : virDomainPtr;
      disk : Interfaces.C.Strings.chars_ptr;
      base : Interfaces.C.Strings.chars_ptr;
      bandwidth : unsigned_long;
      flags : unsigned) return int;  -- /usr/include/libvirt/libvirt-domain.h:2254
   pragma Import (C, virDomainBlockRebase, "virDomainBlockRebase");

  --*
  -- * virDomainBlockCopyFlags:
  -- *
  -- * Flags available for virDomainBlockCopy().
  --  

  -- Limit copy to top of source
  --                                                 backing chain  

  -- Reuse existing external
  --                                                 file for a copy  

   subtype virDomainBlockCopyFlags is unsigned;
   VIR_DOMAIN_BLOCK_COPY_SHALLOW : constant virDomainBlockCopyFlags := 1;
   VIR_DOMAIN_BLOCK_COPY_REUSE_EXT : constant virDomainBlockCopyFlags := 2;  -- /usr/include/libvirt/libvirt-domain.h:2268

  --*
  -- * VIR_DOMAIN_BLOCK_COPY_BANDWIDTH:
  -- * Macro for the virDomainBlockCopy bandwidth tunable: it represents
  -- * the maximum bandwidth in bytes/s, and is used while getting the
  -- * copy operation into the mirrored phase, with a type of ullong.  For
  -- * compatibility with virDomainBlockJobSetSpeed(), values larger than
  -- * 2^52 bytes/sec (a 32-bit MiB/s value) may be rejected on input due
  -- * to overflow considerations (but do you really have an interface
  -- * with that much bandwidth?), and values larger than 2^31 bytes/sec
  -- * may cause overflow problems if queried in bytes/sec.  Hypervisors
  -- * may further restrict the set of valid values. Specifying 0 is the
  -- * same as omitting this parameter, to request no bandwidth limiting.
  -- * Some hypervisors may lack support for this parameter, while still
  -- * allowing a subsequent change of bandwidth via
  -- * virDomainBlockJobSetSpeed().  The actual speed can be determined
  -- * with virDomainGetBlockJobInfo().
  --  

  --*
  -- * VIR_DOMAIN_BLOCK_COPY_GRANULARITY:
  -- * Macro for the virDomainBlockCopy granularity tunable: it represents
  -- * the granularity in bytes at which the copy operation recognizes
  -- * dirty blocks that need copying, as an unsigned int.  Hypervisors may
  -- * restrict this to be a power of two or fall within a certain
  -- * range. Specifying 0 is the same as omitting this parameter, to
  -- * request the hypervisor default.
  --  

  --*
  -- * VIR_DOMAIN_BLOCK_COPY_BUF_SIZE:
  -- * Macro for the virDomainBlockCopy buffer size tunable: it represents
  -- * how much data in bytes can be in flight between source and destination,
  -- * as an unsigned long long. Specifying 0 is the same as omitting this
  -- * parameter, to request the hypervisor default.
  --  

   function virDomainBlockCopy
     (dom : virDomainPtr;
      disk : Interfaces.C.Strings.chars_ptr;
      destxml : Interfaces.C.Strings.chars_ptr;
      params : virTypedParameterPtr;
      nparams : int;
      flags : unsigned) return int;  -- /usr/include/libvirt/libvirt-domain.h:2309
   pragma Import (C, virDomainBlockCopy, "virDomainBlockCopy");

  --*
  -- * virDomainBlockCommitFlags:
  -- *
  -- * Flags available for virDomainBlockCommit().
  --  

  -- NULL base means next backing
  --                                                 file, not whole chain  

  -- Delete any files that are now
  --                                                 invalid after their contents
  --                                                 have been committed  

  -- Allow a two-phase commit when
  --                                                 top is the active layer  

  -- keep the backing chain
  --                                                  referenced using relative
  --                                                  names  

  -- bandwidth in bytes/s
  --                                                         instead of MiB/s  

   subtype virDomainBlockCommitFlags is unsigned;
   VIR_DOMAIN_BLOCK_COMMIT_SHALLOW : constant virDomainBlockCommitFlags := 1;
   VIR_DOMAIN_BLOCK_COMMIT_DELETE : constant virDomainBlockCommitFlags := 2;
   VIR_DOMAIN_BLOCK_COMMIT_ACTIVE : constant virDomainBlockCommitFlags := 4;
   VIR_DOMAIN_BLOCK_COMMIT_RELATIVE : constant virDomainBlockCommitFlags := 8;
   VIR_DOMAIN_BLOCK_COMMIT_BANDWIDTH_BYTES : constant virDomainBlockCommitFlags := 16;  -- /usr/include/libvirt/libvirt-domain.h:2333

   function virDomainBlockCommit
     (dom : virDomainPtr;
      disk : Interfaces.C.Strings.chars_ptr;
      base : Interfaces.C.Strings.chars_ptr;
      top : Interfaces.C.Strings.chars_ptr;
      bandwidth : unsigned_long;
      flags : unsigned) return int;  -- /usr/include/libvirt/libvirt-domain.h:2335
   pragma Import (C, virDomainBlockCommit, "virDomainBlockCommit");

  -- Block I/O throttling support  
  --*
  -- * VIR_DOMAIN_BLOCK_IOTUNE_TOTAL_BYTES_SEC:
  -- *
  -- * Macro for the BlockIoTune tunable weight: it represents the total
  -- * bytes per second permitted through a block device, as a ullong.
  --  

  --*
  -- * VIR_DOMAIN_BLOCK_IOTUNE_READ_BYTES_SEC:
  -- *
  -- * Macro for the BlockIoTune tunable weight: it represents the read
  -- * bytes per second permitted through a block device, as a ullong.
  --  

  --*
  -- * VIR_DOMAIN_BLOCK_IOTUNE_WRITE_BYTES_SEC:
  -- *
  -- * Macro for the BlockIoTune tunable weight: it represents the write
  -- * bytes per second permitted through a block device, as a ullong.
  --  

  --*
  -- * VIR_DOMAIN_BLOCK_IOTUNE_TOTAL_IOPS_SEC:
  -- *
  -- * Macro for the BlockIoTune tunable weight: it represents the total
  -- * I/O operations per second permitted through a block device, as a ullong.
  --  

  --*
  -- * VIR_DOMAIN_BLOCK_IOTUNE_READ_IOPS_SEC:
  -- *
  -- * Macro for the BlockIoTune tunable weight: it represents the read
  -- * I/O operations per second permitted through a block device, as a ullong.
  --  

  --*
  -- * VIR_DOMAIN_BLOCK_IOTUNE_WRITE_IOPS_SEC:
  -- * Macro for the BlockIoTune tunable weight: it represents the write
  -- * I/O operations per second permitted through a block device, as a ullong.
  --  

  --*
  -- * VIR_DOMAIN_BLOCK_IOTUNE_TOTAL_BYTES_SEC_MAX:
  -- *
  -- * Macro for the BlockIoTune tunable weight: it represents the maximum total
  -- * bytes per second permitted through a block device, as a ullong.
  --  

  --*
  -- * VIR_DOMAIN_BLOCK_IOTUNE_READ_BYTES_SEC_MAX:
  -- *
  -- * Macro for the BlockIoTune tunable weight: it represents the maximum read
  -- * bytes per second permitted through a block device, as a ullong.
  --  

  --*
  -- * VIR_DOMAIN_BLOCK_IOTUNE_WRITE_BYTES_SEC_MAX:
  -- *
  -- * Macro for the BlockIoTune tunable weight: it represents the maximum write
  -- * bytes per second permitted through a block device, as a ullong.
  --  

  --*
  -- * VIR_DOMAIN_BLOCK_IOTUNE_TOTAL_IOPS_SEC_MAX:
  -- *
  -- * Macro for the BlockIoTune tunable weight: it represents the maximum
  -- * I/O operations per second permitted through a block device, as a ullong.
  --  

  --*
  -- * VIR_DOMAIN_BLOCK_IOTUNE_READ_IOPS_SEC_MAX:
  -- *
  -- * Macro for the BlockIoTune tunable weight: it represents the maximum read
  -- * I/O operations per second permitted through a block device, as a ullong.
  --  

  --*
  -- * VIR_DOMAIN_BLOCK_IOTUNE_WRITE_IOPS_SEC_MAX:
  -- * Macro for the BlockIoTune tunable weight: it represents the maximum write
  -- * I/O operations per second permitted through a block device, as a ullong.
  --  

  --*
  -- * VIR_DOMAIN_BLOCK_IOTUNE_TOTAL_BYTES_SEC_MAX_LENGTH:
  -- *
  -- * Macro for the BlockIoTune tunable weight: it represents the duration in
  -- * seconds for the burst allowed by total_bytes_sec_max, as a ullong.
  --  

  --*
  -- * VIR_DOMAIN_BLOCK_IOTUNE_READ_BYTES_SEC_MAX_LENGTH:
  -- *
  -- * Macro for the BlockIoTune tunable weight: it represents the duration in
  -- * seconds for the burst allowed by read_bytes_sec_max, as a ullong.
  --  

  --*
  -- * VIR_DOMAIN_BLOCK_IOTUNE_WRITE_BYTES_SEC_MAX_LENGTH:
  -- *
  -- * Macro for the BlockIoTune tunable weight: it represents the duration in
  -- * seconds for the burst allowed by write_bytes_sec_max, as a ullong.
  --  

  --*
  -- * VIR_DOMAIN_BLOCK_IOTUNE_TOTAL_IOPS_SEC_MAX_LENGTH:
  -- *
  -- * Macro for the BlockIoTune tunable weight: it represents the duration in
  -- * seconds for the burst allowed by total_iops_sec_max, as a ullong.
  --  

  --*
  -- * VIR_DOMAIN_BLOCK_IOTUNE_READ_IOPS_SEC_MAX_LENGTH:
  -- *
  -- * Macro for the BlockIoTune tunable weight: it represents the duration in
  -- * seconds for the burst allowed by read_iops_sec_max, as a ullong.
  --  

  --*
  -- * VIR_DOMAIN_BLOCK_IOTUNE_WRITE_IOPS_SEC_MAX_LENGTH:
  -- *
  -- * Macro for the BlockIoTune tunable weight: it represents the duration in
  -- * seconds for the burst allowed by write_iops_sec_max, as a ullong.
  --  

  --*
  -- * VIR_DOMAIN_BLOCK_IOTUNE_SIZE_IOPS_SEC:
  -- * Macro for the BlockIoTune tunable weight: it represents the size
  -- * I/O operations per second permitted through a block device, as a ullong.
  --  

   function virDomainSetBlockIoTune
     (dom : virDomainPtr;
      disk : Interfaces.C.Strings.chars_ptr;
      params : virTypedParameterPtr;
      nparams : int;
      flags : unsigned) return int;  -- /usr/include/libvirt/libvirt-domain.h:2492
   pragma Import (C, virDomainSetBlockIoTune, "virDomainSetBlockIoTune");

   function virDomainGetBlockIoTune
     (dom : virDomainPtr;
      disk : Interfaces.C.Strings.chars_ptr;
      params : virTypedParameterPtr;
      nparams : access int;
      flags : unsigned) return int;  -- /usr/include/libvirt/libvirt-domain.h:2498
   pragma Import (C, virDomainGetBlockIoTune, "virDomainGetBlockIoTune");

  --*
  -- * virDomainDiskErrorCode:
  -- *
  -- * Disk I/O error.
  --  

  -- no error  
  -- unspecified I/O error  
  -- no space left on the device  
   type virDomainDiskErrorCode is 
     (VIR_DOMAIN_DISK_ERROR_NONE,
      VIR_DOMAIN_DISK_ERROR_UNSPEC,
      VIR_DOMAIN_DISK_ERROR_NO_SPACE);
   pragma Convention (C, virDomainDiskErrorCode);  -- /usr/include/libvirt/libvirt-domain.h:2517

  --*
  -- * virDomainDiskError:
  -- *
  --  

   type u_virDomainDiskError;
   subtype virDomainDiskError is u_virDomainDiskError;

   type virDomainDiskErrorPtr is new System.Address;  -- /usr/include/libvirt/libvirt-domain.h:2524

  -- disk target  
   type u_virDomainDiskError is record
      disk : Interfaces.C.Strings.chars_ptr;  -- /usr/include/libvirt/libvirt-domain.h:2527
      error : aliased int;  -- /usr/include/libvirt/libvirt-domain.h:2528
   end record;
   pragma Convention (C_Pass_By_Copy, u_virDomainDiskError);  -- /usr/include/libvirt/libvirt-domain.h:2526

  -- virDomainDiskErrorCode  
   function virDomainGetDiskErrors
     (dom : virDomainPtr;
      errors : virDomainDiskErrorPtr;
      maxerrors : unsigned;
      flags : unsigned) return int;  -- /usr/include/libvirt/libvirt-domain.h:2531
   pragma Import (C, virDomainGetDiskErrors, "virDomainGetDiskErrors");

  --*
  -- * virKeycodeSet:
  -- *
  -- * Enum to specify which keycode mapping is in use for virDomainSendKey().
  --  

  --     * NB: this enum value will increase over time as new events are
  --     * added to the libvirt API. It reflects the last keycode set supported
  --     * by this version of the libvirt API.
  --      

   type virKeycodeSet is 
     (VIR_KEYCODE_SET_LINUX,
      VIR_KEYCODE_SET_XT,
      VIR_KEYCODE_SET_ATSET1,
      VIR_KEYCODE_SET_ATSET2,
      VIR_KEYCODE_SET_ATSET3,
      VIR_KEYCODE_SET_OSX,
      VIR_KEYCODE_SET_XT_KBD,
      VIR_KEYCODE_SET_USB,
      VIR_KEYCODE_SET_WIN32,
      VIR_KEYCODE_SET_RFB);
   pragma Convention (C, virKeycodeSet);  -- /usr/include/libvirt/libvirt-domain.h:2563

  --*
  -- * VIR_DOMAIN_SEND_KEY_MAX_KEYS:
  -- *
  -- * Maximum number of keycodes that can be sent in one virDomainSendKey() call.
  --  

   function virDomainSendKey
     (domain : virDomainPtr;
      codeset : unsigned;
      holdtime : unsigned;
      keycodes : access unsigned;
      nkeycodes : int;
      flags : unsigned) return int;  -- /usr/include/libvirt/libvirt-domain.h:2572
   pragma Import (C, virDomainSendKey, "virDomainSendKey");

  -- * These just happen to match Linux signal numbers. The numbers
  -- * will be mapped to whatever the SIGNUM is in the guest OS in
  -- * question by the agent delivering the signal. The names are
  -- * based on the POSIX / XSI signal standard though.
  -- *
  -- * Do not rely on all values matching Linux though. It is possible
  -- * this enum might be extended with new signals which have no
  -- * mapping in Linux.
  --  

  -- No constant in POSIX/Linux  
  -- SIGHUP  
  -- SIGINT  
  -- SIGQUIT  
  -- SIGILL  
  -- SIGTRAP  
  -- SIGABRT  
  -- SIGBUS  
  -- SIGFPE  
  -- SIGKILL  
  -- SIGUSR1  
  -- SIGSEGV  
  -- SIGUSR2  
  -- SIGPIPE  
  -- SIGALRM  
  -- SIGTERM  
  -- Not in POSIX (SIGSTKFLT on Linux ) 
  -- SIGCHLD  
  -- SIGCONT  
  -- SIGSTOP  
  -- SIGTSTP  
  -- SIGTTIN  
  -- SIGTTOU  
  -- SIGURG  
  -- SIGXCPU  
  -- SIGXFSZ  
  -- SIGVTALRM  
  -- SIGPROF  
  -- Not in POSIX (SIGWINCH on Linux)  
  -- SIGPOLL (also known as SIGIO on Linux)  
  -- Not in POSIX (SIGPWR on Linux)  
  -- SIGSYS (also known as SIGUNUSED on Linux)  
  -- SIGRTMIN  
  -- SIGRTMIN + 1  
  -- SIGRTMIN + 2  
  -- SIGRTMIN + 3  
  -- SIGRTMIN + 4  
  -- SIGRTMIN + 5  
  -- SIGRTMIN + 6  
  -- SIGRTMIN + 7  
  -- SIGRTMIN + 8  
  -- SIGRTMIN + 9  
  -- SIGRTMIN + 10  
  -- SIGRTMIN + 11  
  -- SIGRTMIN + 12  
  -- SIGRTMIN + 13  
  -- SIGRTMIN + 14  
  -- SIGRTMIN + 15  
  -- SIGRTMIN + 16  
  -- SIGRTMIN + 17  
  -- SIGRTMIN + 18  
  -- SIGRTMIN + 19  
  -- SIGRTMIN + 20  
  -- SIGRTMIN + 21  
  -- SIGRTMIN + 22  
  -- SIGRTMIN + 23  
  -- SIGRTMIN + 24  
  -- SIGRTMIN + 25  
  -- SIGRTMIN + 26  
  -- SIGRTMIN + 27  
  -- SIGRTMIN + 28  
  -- SIGRTMIN + 29  
  -- SIGRTMIN + 30  
  -- SIGRTMIN + 31  
  -- SIGRTMIN + 32 / SIGRTMAX  
   type virDomainProcessSignal is 
     (VIR_DOMAIN_PROCESS_SIGNAL_NOP,
      VIR_DOMAIN_PROCESS_SIGNAL_HUP,
      VIR_DOMAIN_PROCESS_SIGNAL_INT,
      VIR_DOMAIN_PROCESS_SIGNAL_QUIT,
      VIR_DOMAIN_PROCESS_SIGNAL_ILL,
      VIR_DOMAIN_PROCESS_SIGNAL_TRAP,
      VIR_DOMAIN_PROCESS_SIGNAL_ABRT,
      VIR_DOMAIN_PROCESS_SIGNAL_BUS,
      VIR_DOMAIN_PROCESS_SIGNAL_FPE,
      VIR_DOMAIN_PROCESS_SIGNAL_KILL,
      VIR_DOMAIN_PROCESS_SIGNAL_USR1,
      VIR_DOMAIN_PROCESS_SIGNAL_SEGV,
      VIR_DOMAIN_PROCESS_SIGNAL_USR2,
      VIR_DOMAIN_PROCESS_SIGNAL_PIPE,
      VIR_DOMAIN_PROCESS_SIGNAL_ALRM,
      VIR_DOMAIN_PROCESS_SIGNAL_TERM,
      VIR_DOMAIN_PROCESS_SIGNAL_STKFLT,
      VIR_DOMAIN_PROCESS_SIGNAL_CHLD,
      VIR_DOMAIN_PROCESS_SIGNAL_CONT,
      VIR_DOMAIN_PROCESS_SIGNAL_STOP,
      VIR_DOMAIN_PROCESS_SIGNAL_TSTP,
      VIR_DOMAIN_PROCESS_SIGNAL_TTIN,
      VIR_DOMAIN_PROCESS_SIGNAL_TTOU,
      VIR_DOMAIN_PROCESS_SIGNAL_URG,
      VIR_DOMAIN_PROCESS_SIGNAL_XCPU,
      VIR_DOMAIN_PROCESS_SIGNAL_XFSZ,
      VIR_DOMAIN_PROCESS_SIGNAL_VTALRM,
      VIR_DOMAIN_PROCESS_SIGNAL_PROF,
      VIR_DOMAIN_PROCESS_SIGNAL_WINCH,
      VIR_DOMAIN_PROCESS_SIGNAL_POLL,
      VIR_DOMAIN_PROCESS_SIGNAL_PWR,
      VIR_DOMAIN_PROCESS_SIGNAL_SYS,
      VIR_DOMAIN_PROCESS_SIGNAL_RT0,
      VIR_DOMAIN_PROCESS_SIGNAL_RT1,
      VIR_DOMAIN_PROCESS_SIGNAL_RT2,
      VIR_DOMAIN_PROCESS_SIGNAL_RT3,
      VIR_DOMAIN_PROCESS_SIGNAL_RT4,
      VIR_DOMAIN_PROCESS_SIGNAL_RT5,
      VIR_DOMAIN_PROCESS_SIGNAL_RT6,
      VIR_DOMAIN_PROCESS_SIGNAL_RT7,
      VIR_DOMAIN_PROCESS_SIGNAL_RT8,
      VIR_DOMAIN_PROCESS_SIGNAL_RT9,
      VIR_DOMAIN_PROCESS_SIGNAL_RT10,
      VIR_DOMAIN_PROCESS_SIGNAL_RT11,
      VIR_DOMAIN_PROCESS_SIGNAL_RT12,
      VIR_DOMAIN_PROCESS_SIGNAL_RT13,
      VIR_DOMAIN_PROCESS_SIGNAL_RT14,
      VIR_DOMAIN_PROCESS_SIGNAL_RT15,
      VIR_DOMAIN_PROCESS_SIGNAL_RT16,
      VIR_DOMAIN_PROCESS_SIGNAL_RT17,
      VIR_DOMAIN_PROCESS_SIGNAL_RT18,
      VIR_DOMAIN_PROCESS_SIGNAL_RT19,
      VIR_DOMAIN_PROCESS_SIGNAL_RT20,
      VIR_DOMAIN_PROCESS_SIGNAL_RT21,
      VIR_DOMAIN_PROCESS_SIGNAL_RT22,
      VIR_DOMAIN_PROCESS_SIGNAL_RT23,
      VIR_DOMAIN_PROCESS_SIGNAL_RT24,
      VIR_DOMAIN_PROCESS_SIGNAL_RT25,
      VIR_DOMAIN_PROCESS_SIGNAL_RT26,
      VIR_DOMAIN_PROCESS_SIGNAL_RT27,
      VIR_DOMAIN_PROCESS_SIGNAL_RT28,
      VIR_DOMAIN_PROCESS_SIGNAL_RT29,
      VIR_DOMAIN_PROCESS_SIGNAL_RT30,
      VIR_DOMAIN_PROCESS_SIGNAL_RT31,
      VIR_DOMAIN_PROCESS_SIGNAL_RT32);
   pragma Convention (C, virDomainProcessSignal);  -- /usr/include/libvirt/libvirt-domain.h:2665

   function virDomainSendProcessSignal
     (domain : virDomainPtr;
      pid_value : Long_Long_Integer;
      signum : unsigned;
      flags : unsigned) return int;  -- /usr/include/libvirt/libvirt-domain.h:2667
   pragma Import (C, virDomainSendProcessSignal, "virDomainSendProcessSignal");

  -- * Deprecated calls
  --  

   function virDomainCreateLinux
     (conn : VirConnectPtr;
      xmlDesc : Interfaces.C.Strings.chars_ptr;
      flags : unsigned) return virDomainPtr;  -- /usr/include/libvirt/libvirt-domain.h:2675
   pragma Import (C, virDomainCreateLinux, "virDomainCreateLinux");

  -- * Domain Event Notification
  --  

  --*
  -- * virDomainEventType:
  -- *
  -- * a virDomainEventType is emitted during domain lifecycle events
  --  

   type virDomainEventType is 
     (VIR_DOMAIN_EVENT_DEFINED,
      VIR_DOMAIN_EVENT_UNDEFINED,
      VIR_DOMAIN_EVENT_STARTED,
      VIR_DOMAIN_EVENT_SUSPENDED,
      VIR_DOMAIN_EVENT_RESUMED,
      VIR_DOMAIN_EVENT_STOPPED,
      VIR_DOMAIN_EVENT_SHUTDOWN,
      VIR_DOMAIN_EVENT_PMSUSPENDED,
      VIR_DOMAIN_EVENT_CRASHED);
   pragma Convention (C, virDomainEventType);  -- /usr/include/libvirt/libvirt-domain.h:2703

  --*
  -- * virDomainEventDefinedDetailType:
  -- *
  -- * Details on the cause of a 'defined' lifecycle event
  --  

  -- Newly created config file  
  -- Changed config file  
  -- Domain was renamed  
  -- Config was restored from a snapshot  
   type virDomainEventDefinedDetailType is 
     (VIR_DOMAIN_EVENT_DEFINED_ADDED,
      VIR_DOMAIN_EVENT_DEFINED_UPDATED,
      VIR_DOMAIN_EVENT_DEFINED_RENAMED,
      VIR_DOMAIN_EVENT_DEFINED_FROM_SNAPSHOT);
   pragma Convention (C, virDomainEventDefinedDetailType);  -- /usr/include/libvirt/libvirt-domain.h:2719

  --*
  -- * virDomainEventUndefinedDetailType:
  -- *
  -- * Details on the cause of an 'undefined' lifecycle event
  --  

  -- Deleted the config file  
  -- Domain was renamed  
   type virDomainEventUndefinedDetailType is 
     (VIR_DOMAIN_EVENT_UNDEFINED_REMOVED,
      VIR_DOMAIN_EVENT_UNDEFINED_RENAMED);
   pragma Convention (C, virDomainEventUndefinedDetailType);  -- /usr/include/libvirt/libvirt-domain.h:2733

  --*
  -- * virDomainEventStartedDetailType:
  -- *
  -- * Details on the cause of a 'started' lifecycle event
  --  

  -- Normal startup from boot  
  -- Incoming migration from another host  
  -- Restored from a state file  
  -- Restored from snapshot  
  -- Started due to wakeup event  
   type virDomainEventStartedDetailType is 
     (VIR_DOMAIN_EVENT_STARTED_BOOTED,
      VIR_DOMAIN_EVENT_STARTED_MIGRATED,
      VIR_DOMAIN_EVENT_STARTED_RESTORED,
      VIR_DOMAIN_EVENT_STARTED_FROM_SNAPSHOT,
      VIR_DOMAIN_EVENT_STARTED_WAKEUP);
   pragma Convention (C, virDomainEventStartedDetailType);  -- /usr/include/libvirt/libvirt-domain.h:2750

  --*
  -- * virDomainEventSuspendedDetailType:
  -- *
  -- * Details on the cause of a 'suspended' lifecycle event
  --  

  -- Normal suspend due to admin pause  
  -- Suspended for offline migration  
  -- Suspended due to a disk I/O error  
  -- Suspended due to a watchdog firing  
  -- Restored from paused state file  
  -- Restored from paused snapshot  
  -- suspended after failure during libvirt API call  
  -- suspended for post-copy migration  
  -- suspended after failed post-copy  
   type virDomainEventSuspendedDetailType is 
     (VIR_DOMAIN_EVENT_SUSPENDED_PAUSED,
      VIR_DOMAIN_EVENT_SUSPENDED_MIGRATED,
      VIR_DOMAIN_EVENT_SUSPENDED_IOERROR,
      VIR_DOMAIN_EVENT_SUSPENDED_WATCHDOG,
      VIR_DOMAIN_EVENT_SUSPENDED_RESTORED,
      VIR_DOMAIN_EVENT_SUSPENDED_FROM_SNAPSHOT,
      VIR_DOMAIN_EVENT_SUSPENDED_API_ERROR,
      VIR_DOMAIN_EVENT_SUSPENDED_POSTCOPY,
      VIR_DOMAIN_EVENT_SUSPENDED_POSTCOPY_FAILED);
   pragma Convention (C, virDomainEventSuspendedDetailType);  -- /usr/include/libvirt/libvirt-domain.h:2771

  --*
  -- * virDomainEventResumedDetailType:
  -- *
  -- * Details on the cause of a 'resumed' lifecycle event
  --  

  -- Normal resume due to admin unpause  
  -- Resumed for completion of migration  
  -- Resumed from snapshot  
  -- Resumed, but migration is still
  --                                                running in post-copy mode  

   type virDomainEventResumedDetailType is 
     (VIR_DOMAIN_EVENT_RESUMED_UNPAUSED,
      VIR_DOMAIN_EVENT_RESUMED_MIGRATED,
      VIR_DOMAIN_EVENT_RESUMED_FROM_SNAPSHOT,
      VIR_DOMAIN_EVENT_RESUMED_POSTCOPY);
   pragma Convention (C, virDomainEventResumedDetailType);  -- /usr/include/libvirt/libvirt-domain.h:2788

  --*
  -- * virDomainEventStoppedDetailType:
  -- *
  -- * Details on the cause of a 'stopped' lifecycle event
  --  

  -- Normal shutdown  
  -- Forced poweroff from host  
  -- Guest crashed  
  -- Migrated off to another host  
  -- Saved to a state file  
  -- Host emulator/mgmt failed  
  -- offline snapshot loaded  
   type virDomainEventStoppedDetailType is 
     (VIR_DOMAIN_EVENT_STOPPED_SHUTDOWN,
      VIR_DOMAIN_EVENT_STOPPED_DESTROYED,
      VIR_DOMAIN_EVENT_STOPPED_CRASHED,
      VIR_DOMAIN_EVENT_STOPPED_MIGRATED,
      VIR_DOMAIN_EVENT_STOPPED_SAVED,
      VIR_DOMAIN_EVENT_STOPPED_FAILED,
      VIR_DOMAIN_EVENT_STOPPED_FROM_SNAPSHOT);
   pragma Convention (C, virDomainEventStoppedDetailType);  -- /usr/include/libvirt/libvirt-domain.h:2807

  --*
  -- * virDomainEventShutdownDetailType:
  -- *
  -- * Details on the cause of a 'shutdown' lifecycle event
  --  

  -- Guest finished shutdown sequence  
   type virDomainEventShutdownDetailType is 
     (VIR_DOMAIN_EVENT_SHUTDOWN_FINISHED);
   pragma Convention (C, virDomainEventShutdownDetailType);  -- /usr/include/libvirt/libvirt-domain.h:2821

  --*
  -- * virDomainEventPMSuspendedDetailType:
  -- *
  -- * Details on the cause of a 'pmsuspended' lifecycle event
  --  

  -- Guest was PM suspended to memory  
  -- Guest was PM suspended to disk  
   type virDomainEventPMSuspendedDetailType is 
     (VIR_DOMAIN_EVENT_PMSUSPENDED_MEMORY,
      VIR_DOMAIN_EVENT_PMSUSPENDED_DISK);
   pragma Convention (C, virDomainEventPMSuspendedDetailType);  -- /usr/include/libvirt/libvirt-domain.h:2835

  --*
  -- * virDomainEventCrashedDetailType:
  -- *
  -- * Details on the cause of a 'crashed' lifecycle event
  --  

  -- Guest was panicked  
   type virDomainEventCrashedDetailType is 
     (VIR_DOMAIN_EVENT_CRASHED_PANICKED);
   pragma Convention (C, virDomainEventCrashedDetailType);  -- /usr/include/libvirt/libvirt-domain.h:2848

  --*
  -- * virConnectDomainEventCallback:
  -- * @conn: virConnect connection
  -- * @dom: The domain on which the event occurred
  -- * @event: The specific virDomainEventType which occurred
  -- * @detail: event specific detail information
  -- * @opaque: opaque user data
  -- *
  -- * A callback function to be registered, and called when a domain event occurs
  -- *
  -- * Returns 0 (the return value is currently ignored)
  --  

   type virConnectDomainEventCallback is access function
     (conn   : VirConnectPtr;
      dom    : virDomainPtr;
      event  : int;
      detail : int;
      opaque : System.Address) return int;
   pragma Convention (C, virConnectDomainEventCallback);  -- /usr/include/libvirt/libvirt-domain.h:2862

   function virConnectDomainEventRegister
     (conn : VirConnectPtr;
      cb : virConnectDomainEventCallback;
      opaque : System.Address;
      freecb : virFreeCallback) return int;  -- /usr/include/libvirt/libvirt-domain.h:2868
   pragma Import (C, virConnectDomainEventRegister, "virConnectDomainEventRegister");

   function virConnectDomainEventDeregister (conn : VirConnectPtr; cb : virConnectDomainEventCallback) return int;  -- /usr/include/libvirt/libvirt-domain.h:2873
   pragma Import (C, virConnectDomainEventDeregister, "virConnectDomainEventDeregister");

   function virDomainIsActive (dom : virDomainPtr) return int;  -- /usr/include/libvirt/libvirt-domain.h:2877
   pragma Import (C, virDomainIsActive, "virDomainIsActive");

   function virDomainIsPersistent (dom : virDomainPtr) return int;  -- /usr/include/libvirt/libvirt-domain.h:2878
   pragma Import (C, virDomainIsPersistent, "virDomainIsPersistent");

   function virDomainIsUpdated (dom : virDomainPtr) return int;  -- /usr/include/libvirt/libvirt-domain.h:2879
   pragma Import (C, virDomainIsUpdated, "virDomainIsUpdated");

  -- No job is active  
  -- Job with a finite completion time  
  -- Job without a finite completion time  
  -- Job has finished, but isn't cleaned up  
  -- Job hit error, but isn't cleaned up  
  -- Job was aborted, but isn't cleaned up  
   type virDomainJobType is 
     (VIR_DOMAIN_JOB_NONE,
      VIR_DOMAIN_JOB_BOUNDED,
      VIR_DOMAIN_JOB_UNBOUNDED,
      VIR_DOMAIN_JOB_COMPLETED,
      VIR_DOMAIN_JOB_FAILED,
      VIR_DOMAIN_JOB_CANCELLED);
   pragma Convention (C, virDomainJobType);  -- /usr/include/libvirt/libvirt-domain.h:2892

   type u_virDomainJobInfo;
   subtype virDomainJobInfo is u_virDomainJobInfo;

   type virDomainJobInfoPtr is new System.Address;  -- /usr/include/libvirt/libvirt-domain.h:2895

  -- One of virDomainJobType  
   type u_virDomainJobInfo is record
      c_type : aliased int;  -- /usr/include/libvirt/libvirt-domain.h:2898
      timeElapsed : aliased Extensions.unsigned_long_long;  -- /usr/include/libvirt/libvirt-domain.h:2901
      timeRemaining : aliased Extensions.unsigned_long_long;  -- /usr/include/libvirt/libvirt-domain.h:2902
      dataTotal : aliased Extensions.unsigned_long_long;  -- /usr/include/libvirt/libvirt-domain.h:2915
      dataProcessed : aliased Extensions.unsigned_long_long;  -- /usr/include/libvirt/libvirt-domain.h:2916
      dataRemaining : aliased Extensions.unsigned_long_long;  -- /usr/include/libvirt/libvirt-domain.h:2917
      memTotal : aliased Extensions.unsigned_long_long;  -- /usr/include/libvirt/libvirt-domain.h:2920
      memProcessed : aliased Extensions.unsigned_long_long;  -- /usr/include/libvirt/libvirt-domain.h:2921
      memRemaining : aliased Extensions.unsigned_long_long;  -- /usr/include/libvirt/libvirt-domain.h:2922
      fileTotal : aliased Extensions.unsigned_long_long;  -- /usr/include/libvirt/libvirt-domain.h:2925
      fileProcessed : aliased Extensions.unsigned_long_long;  -- /usr/include/libvirt/libvirt-domain.h:2926
      fileRemaining : aliased Extensions.unsigned_long_long;  -- /usr/include/libvirt/libvirt-domain.h:2927
   end record;
   pragma Convention (C_Pass_By_Copy, u_virDomainJobInfo);  -- /usr/include/libvirt/libvirt-domain.h:2896

  -- Time is measured in milliseconds  
  -- Always set  
  -- Only for VIR_DOMAIN_JOB_BOUNDED  
  -- Data is measured in bytes unless otherwise specified
  --     * and is measuring the job as a whole.
  --     *
  --     * For VIR_DOMAIN_JOB_UNBOUNDED, dataTotal may be less
  --     * than the final sum of dataProcessed + dataRemaining
  --     * in the event that the hypervisor has to repeat some
  --     * data, such as due to dirtied pages during migration.
  --     *
  --     * For VIR_DOMAIN_JOB_BOUNDED, dataTotal shall always
  --     * equal the sum of dataProcessed + dataRemaining.
  --      

  -- As above, but only tracking guest memory progress  
  -- As above, but only tracking guest disk file progress  
  --*
  -- * virDomainGetJobStatsFlags:
  -- *
  -- * Flags OR'ed together to provide specific behavior when querying domain
  -- * job statistics.
  --  

  -- return stats of a recently
  --                                              * completed job  

   subtype virDomainGetJobStatsFlags is unsigned;
   VIR_DOMAIN_JOB_STATS_COMPLETED : constant virDomainGetJobStatsFlags := 1;  -- /usr/include/libvirt/libvirt-domain.h:2939

   function virDomainGetJobInfo (dom : virDomainPtr; info : virDomainJobInfoPtr) return int;  -- /usr/include/libvirt/libvirt-domain.h:2941
   pragma Import (C, virDomainGetJobInfo, "virDomainGetJobInfo");

   function virDomainGetJobStats
     (domain : virDomainPtr;
      c_type : access int;
      params : System.Address;
      nparams : access int;
      flags : unsigned) return int;  -- /usr/include/libvirt/libvirt-domain.h:2943
   pragma Import (C, virDomainGetJobStats, "virDomainGetJobStats");

   function virDomainAbortJob (dom : virDomainPtr) return int;  -- /usr/include/libvirt/libvirt-domain.h:2948
   pragma Import (C, virDomainAbortJob, "virDomainAbortJob");

  --*
  -- * VIR_DOMAIN_JOB_TIME_ELAPSED:
  -- *
  -- * virDomainGetJobStats field: time (ms) since the beginning of the
  -- * job, as VIR_TYPED_PARAM_ULLONG.
  -- *
  -- * This field corresponds to timeElapsed field in virDomainJobInfo.
  --  

  --*
  -- * VIR_DOMAIN_JOB_TIME_ELAPSED_NET:
  -- *
  -- * virDomainGetJobStats field: time (ms) since the beginning of the
  -- * migration job NOT including the time required to transfer control
  -- * flow from the source host to the destination host,
  -- * as VIR_TYPED_PARAM_ULLONG.
  --  

  --*
  -- * VIR_DOMAIN_JOB_TIME_REMAINING:
  -- *
  -- * virDomainGetJobStats field: remaining time (ms) for VIR_DOMAIN_JOB_BOUNDED
  -- * jobs, as VIR_TYPED_PARAM_ULLONG.
  -- *
  -- * This field corresponds to timeRemaining field in virDomainJobInfo.
  --  

  --*
  -- * VIR_DOMAIN_JOB_DOWNTIME:
  -- *
  -- * virDomainGetJobStats field: downtime (ms) that is expected to happen
  -- * during migration, as VIR_TYPED_PARAM_ULLONG. The real computed downtime
  -- * between the time guest CPUs were paused and the time they were resumed
  -- * is reported for completed migration.
  --  

  --*
  -- * VIR_DOMAIN_JOB_DOWNTIME_NET:
  -- *
  -- * virDomainGetJobStats field: real measured downtime (ms) NOT including
  -- * the time required to transfer control flow from the source host to the
  -- * destination host, as VIR_TYPED_PARAM_ULLONG.
  --  

  --*
  -- * VIR_DOMAIN_JOB_SETUP_TIME:
  -- *
  -- * virDomainGetJobStats field: total time in milliseconds spent preparing
  -- * the migration in the 'setup' phase before the iterations begin, as
  -- * VIR_TYPED_PARAM_ULLONG.
  --  

  --*
  -- * VIR_DOMAIN_JOB_DATA_TOTAL:
  -- *
  -- * virDomainGetJobStats field: total number of bytes supposed to be
  -- * transferred, as VIR_TYPED_PARAM_ULLONG. For VIR_DOMAIN_JOB_UNBOUNDED
  -- * jobs, this may be less than the sum of VIR_DOMAIN_JOB_DATA_PROCESSED and
  -- * VIR_DOMAIN_JOB_DATA_REMAINING in the event that the hypervisor has to
  -- * repeat some data, e.g., due to dirtied pages during migration. For
  -- * VIR_DOMAIN_JOB_BOUNDED jobs, VIR_DOMAIN_JOB_DATA_TOTAL shall always equal
  -- * VIR_DOMAIN_JOB_DATA_PROCESSED + VIR_DOMAIN_JOB_DATA_REMAINING.
  -- *
  -- * This field corresponds to dataTotal field in virDomainJobInfo.
  --  

  --*
  -- * VIR_DOMAIN_JOB_DATA_PROCESSED:
  -- *
  -- * virDomainGetJobStats field: number of bytes transferred from the
  -- * beginning of the job, as VIR_TYPED_PARAM_ULLONG.
  -- *
  -- * This field corresponds to dataProcessed field in virDomainJobInfo.
  --  

  --*
  -- * VIR_DOMAIN_JOB_DATA_REMAINING:
  -- *
  -- * virDomainGetJobStats field: number of bytes that still need to be
  -- * transferred, as VIR_TYPED_PARAM_ULLONG.
  -- *
  -- * This field corresponds to dataRemaining field in virDomainJobInfo.
  --  

  --*
  -- * VIR_DOMAIN_JOB_MEMORY_TOTAL:
  -- *
  -- * virDomainGetJobStats field: as VIR_DOMAIN_JOB_DATA_TOTAL but only
  -- * tracking guest memory progress, as VIR_TYPED_PARAM_ULLONG.
  -- *
  -- * This field corresponds to memTotal field in virDomainJobInfo.
  --  

  --*
  -- * VIR_DOMAIN_JOB_MEMORY_PROCESSED:
  -- *
  -- * virDomainGetJobStats field: as VIR_DOMAIN_JOB_DATA_PROCESSED but only
  -- * tracking guest memory progress, as VIR_TYPED_PARAM_ULLONG.
  -- *
  -- * This field corresponds to memProcessed field in virDomainJobInfo.
  --  

  --*
  -- * VIR_DOMAIN_JOB_MEMORY_REMAINING:
  -- *
  -- * virDomainGetJobStats field: as VIR_DOMAIN_JOB_DATA_REMAINING but only
  -- * tracking guest memory progress, as VIR_TYPED_PARAM_ULLONG.
  -- *
  -- * This field corresponds to memRemaining field in virDomainJobInfo.
  --  

  --*
  -- * VIR_DOMAIN_JOB_MEMORY_CONSTANT:
  -- *
  -- * virDomainGetJobStats field: number of pages filled with a constant
  -- * byte (all bytes in a single page are identical) transferred since the
  -- * beginning of the migration job, as VIR_TYPED_PARAM_ULLONG.
  -- *
  -- * The most common example of such pages are zero pages, i.e., pages filled
  -- * with zero bytes.
  --  

  --*
  -- * VIR_DOMAIN_JOB_MEMORY_NORMAL:
  -- *
  -- * virDomainGetJobStats field: number of pages that were transferred without
  -- * any kind of compression (i.e., pages which were not filled with a constant
  -- * byte and which could not be compressed) transferred since the beginning
  -- * of the migration job, as VIR_TYPED_PARAM_ULLONG.
  --  

  --*
  -- * VIR_DOMAIN_JOB_MEMORY_NORMAL_BYTES:
  -- *
  -- * virDomainGetJobStats field: number of bytes transferred as normal pages,
  -- * as VIR_TYPED_PARAM_ULLONG.
  -- *
  -- * See VIR_DOMAIN_JOB_MEMORY_NORMAL for more details.
  --  

  --*
  -- * VIR_DOMAIN_JOB_MEMORY_BPS:
  -- *
  -- * virDomainGetJobStats field: network throughput used while migrating
  -- * memory in Bytes per second, as VIR_TYPED_PARAM_ULLONG.
  --  

  --* VIR_DOMAIN_JOB_MEMORY_DIRTY_RATE:
  -- *
  -- * virDomainGetJobStats field: number of memory pages dirtied by the guest
  -- * per second, as VIR_TYPED_PARAM_ULLONG. This statistics makes sense only
  -- * when live migration is running.
  --  

  --*
  -- * VIR_DOMAIN_JOB_MEMORY_ITERATION:
  -- *
  -- * virDomainGetJobStats field: current iteration over domain's memory
  -- * during live migration, as VIR_TYPED_PARAM_ULLONG. This is set to zero
  -- * when memory starts to be transferred and the value is increased by one
  -- * every time a new iteration is started to transfer memory pages dirtied
  -- * since the last iteration.
  --  

  --*
  -- * VIR_DOMAIN_JOB_DISK_TOTAL:
  -- *
  -- * virDomainGetJobStats field: as VIR_DOMAIN_JOB_DATA_TOTAL but only
  -- * tracking guest disk progress, as VIR_TYPED_PARAM_ULLONG.
  -- *
  -- * This field corresponds to fileTotal field in virDomainJobInfo.
  --  

  --*
  -- * VIR_DOMAIN_JOB_DISK_PROCESSED:
  -- *
  -- * virDomainGetJobStats field: as VIR_DOMAIN_JOB_DATA_PROCESSED but only
  -- * tracking guest disk progress, as VIR_TYPED_PARAM_ULLONG.
  -- *
  -- * This field corresponds to fileProcessed field in virDomainJobInfo.
  --  

  --*
  -- * VIR_DOMAIN_JOB_DISK_REMAINING:
  -- *
  -- * virDomainGetJobStats field: as VIR_DOMAIN_JOB_DATA_REMAINING but only
  -- * tracking guest disk progress, as VIR_TYPED_PARAM_ULLONG.
  -- *
  -- * This field corresponds to fileRemaining field in virDomainJobInfo.
  --  

  --*
  -- * VIR_DOMAIN_JOB_DISK_BPS:
  -- *
  -- * virDomainGetJobStats field: network throughput used while migrating
  -- * disks in Bytes per second, as VIR_TYPED_PARAM_ULLONG.
  --  

  --*
  -- * VIR_DOMAIN_JOB_COMPRESSION_CACHE:
  -- *
  -- * virDomainGetJobStats field: size of the cache (in bytes) used for
  -- * compressing repeatedly transferred memory pages during live migration,
  -- * as VIR_TYPED_PARAM_ULLONG.
  --  

  --*
  -- * VIR_DOMAIN_JOB_COMPRESSION_BYTES:
  -- *
  -- * virDomainGetJobStats field: number of compressed bytes transferred
  -- * since the beginning of migration, as VIR_TYPED_PARAM_ULLONG.
  --  

  --*
  -- * VIR_DOMAIN_JOB_COMPRESSION_PAGES:
  -- *
  -- * virDomainGetJobStats field: number of compressed pages transferred
  -- * since the beginning of migration, as VIR_TYPED_PARAM_ULLONG.
  --  

  --*
  -- * VIR_DOMAIN_JOB_COMPRESSION_CACHE_MISSES:
  -- *
  -- * virDomainGetJobStats field: number of repeatedly changing pages that
  -- * were not found in compression cache and thus could not be compressed,
  -- * as VIR_TYPED_PARAM_ULLONG.
  --  

  --*
  -- * VIR_DOMAIN_JOB_COMPRESSION_OVERFLOW:
  -- *
  -- * virDomainGetJobStats field: number of repeatedly changing pages that
  -- * were found in compression cache but were sent uncompressed because
  -- * the result of compression was larger than the original page as a whole,
  -- * as VIR_TYPED_PARAM_ULLONG.
  --  

  --*
  -- * VIR_DOMAIN_JOB_AUTO_CONVERGE_THROTTLE:
  -- *
  -- * virDomainGetJobStats field: current percentage guest CPUs are throttled
  -- * to when auto-convergence decided migration was not converging, as
  -- * VIR_TYPED_PARAM_INT.
  --  

  --*
  -- * virConnectDomainEventGenericCallback:
  -- * @conn: the connection pointer
  -- * @dom: the domain pointer
  -- * @opaque: application specified data
  -- *
  -- * A generic domain event callback handler, for use with
  -- * virConnectDomainEventRegisterAny(). Specific events usually
  -- * have a customization with extra parameters, often with @opaque being
  -- * passed in a different parameter position; use VIR_DOMAIN_EVENT_CALLBACK()
  -- * when registering an appropriate handler.
  --  

   type virConnectDomainEventGenericCallback is access procedure
     (conn   : VirConnectPtr;
      dom    : virDomainPtr;
      opaque : System.Address);
   pragma Convention (C, virConnectDomainEventGenericCallback);  -- /usr/include/libvirt/libvirt-domain.h:3236

  --*
  -- * virConnectDomainEventRTCChangeCallback:
  -- * @conn: connection object
  -- * @dom: domain on which the event occurred
  -- * @utcoffset: the new RTC offset from UTC, measured in seconds
  -- * @opaque: application specified data
  -- *
  -- * The callback signature to use when registering for an event of type
  -- * VIR_DOMAIN_EVENT_ID_RTC_CHANGE with virConnectDomainEventRegisterAny()
  --  

   type virConnectDomainEventRTCChangeCallback is access procedure
     (conn      : VirConnectPtr;
      dom       : virDomainPtr;
      utcoffset : Long_Long_Integer;
      opaque    : System.Address);
   pragma Convention (C, virConnectDomainEventRTCChangeCallback);  -- /usr/include/libvirt/libvirt-domain.h:3250

  --*
  -- * virDomainEventWatchdogAction:
  -- *
  -- * The action that is to be taken due to the watchdog device firing
  --  

  -- No action, watchdog ignored  
  -- Guest CPUs are paused  
  -- Guest CPUs are reset  
  -- Guest is forcibly powered off  
  -- Guest is requested to gracefully shutdown  
  -- No action, a debug message logged  
  -- Inject a non-maskable interrupt into guest  
   type virDomainEventWatchdogAction is 
     (VIR_DOMAIN_EVENT_WATCHDOG_NONE,
      VIR_DOMAIN_EVENT_WATCHDOG_PAUSE,
      VIR_DOMAIN_EVENT_WATCHDOG_RESET,
      VIR_DOMAIN_EVENT_WATCHDOG_POWEROFF,
      VIR_DOMAIN_EVENT_WATCHDOG_SHUTDOWN,
      VIR_DOMAIN_EVENT_WATCHDOG_DEBUG,
      VIR_DOMAIN_EVENT_WATCHDOG_INJECTNMI);
   pragma Convention (C, virDomainEventWatchdogAction);  -- /usr/include/libvirt/libvirt-domain.h:3272

  --*
  -- * virConnectDomainEventWatchdogCallback:
  -- * @conn: connection object
  -- * @dom: domain on which the event occurred
  -- * @action: action that is to be taken due to watchdog firing
  -- * @opaque: application specified data
  -- *
  -- * The callback signature to use when registering for an event of type
  -- * VIR_DOMAIN_EVENT_ID_WATCHDOG with virConnectDomainEventRegisterAny()
  -- *
  --  

   type virConnectDomainEventWatchdogCallback is access procedure
        (conn : VirConnectPtr;
         dom  : virDomainPtr;
         action : int;
         opaque : System.Address);
   pragma Convention (C, virConnectDomainEventWatchdogCallback);  -- /usr/include/libvirt/libvirt-domain.h:3285

  --*
  -- * virDomainEventIOErrorAction:
  -- *
  -- * The action that is to be taken due to an IO error occurring
  --  

  -- No action, IO error ignored  
  -- Guest CPUs are paused  
  -- IO error reported to guest OS  
   type virDomainEventIOErrorAction is 
     (VIR_DOMAIN_EVENT_IO_ERROR_NONE,
      VIR_DOMAIN_EVENT_IO_ERROR_PAUSE,
      VIR_DOMAIN_EVENT_IO_ERROR_REPORT);
   pragma Convention (C, virDomainEventIOErrorAction);  -- /usr/include/libvirt/libvirt-domain.h:3303

  --*
  -- * virConnectDomainEventIOErrorCallback:
  -- * @conn: connection object
  -- * @dom: domain on which the event occurred
  -- * @srcPath: The host file on which the IO error occurred
  -- * @devAlias: The guest device alias associated with the path
  -- * @action: action that is to be taken due to the IO error
  -- * @opaque: application specified data
  -- *
  -- * The callback signature to use when registering for an event of type
  -- * VIR_DOMAIN_EVENT_ID_IO_ERROR with virConnectDomainEventRegisterAny()
  --  

   type virConnectDomainEventIOErrorCallback is access procedure
     (conn     : VirConnectPtr;
      dom      : virDomainPtr;
      srcPath  : Interfaces.C.Strings.chars_ptr;
      devAlias : Interfaces.C.Strings.chars_ptr;
      action   : int;
      opaque   : System.Address);
   pragma Convention (C, virConnectDomainEventIOErrorCallback);  -- /usr/include/libvirt/libvirt-domain.h:3318

  --*
  -- * virConnectDomainEventIOErrorReasonCallback:
  -- * @conn: connection object
  -- * @dom: domain on which the event occurred
  -- * @srcPath: The host file on which the IO error occurred
  -- * @devAlias: The guest device alias associated with the path
  -- * @action: action that is to be taken due to the IO error
  -- * @reason: the cause of the IO error
  -- * @opaque: application specified data
  -- *
  -- * The callback signature to use when registering for an event of type
  -- * VIR_DOMAIN_EVENT_ID_IO_ERROR_REASON with virConnectDomainEventRegisterAny()
  -- *
  -- * If the I/O error is known to be caused by an ENOSPC condition in
  -- * the host (where resizing the disk to be larger will allow the guest
  -- * to be resumed as if nothing happened), @reason will be "enospc".
  -- * Otherwise, @reason will be "", although future strings may be added
  -- * if determination of other error types becomes possible.
  -- *
  --  

   type virConnectDomainEventIOErrorReasonCallback is access procedure
        (arg1 : VirConnectPtr;
         arg2 : virDomainPtr;
         arg3 : Interfaces.C.Strings.chars_ptr;
         arg4 : Interfaces.C.Strings.chars_ptr;
         arg5 : int;
         arg6 : Interfaces.C.Strings.chars_ptr;
         arg7 : System.Address);
   pragma Convention (C, virConnectDomainEventIOErrorReasonCallback);  -- /usr/include/libvirt/libvirt-domain.h:3345

  --*
  -- * virDomainEventGraphicsPhase:
  -- *
  -- * The phase of the graphics client connection
  --  

  -- Initial socket connection established  
  -- Authentication & setup completed  
  -- Final socket disconnection  
   type virDomainEventGraphicsPhase is 
     (VIR_DOMAIN_EVENT_GRAPHICS_CONNECT,
      VIR_DOMAIN_EVENT_GRAPHICS_INITIALIZE,
      VIR_DOMAIN_EVENT_GRAPHICS_DISCONNECT);
   pragma Convention (C, virDomainEventGraphicsPhase);  -- /usr/include/libvirt/libvirt-domain.h:3366

  --*
  -- * virDomainEventGraphicsAddressType:
  -- *
  -- * The type of address for the connection
  --  

  -- IPv4 address  
  -- IPv6 address  
  -- UNIX socket path  
   type virDomainEventGraphicsAddressType is 
     (VIR_DOMAIN_EVENT_GRAPHICS_ADDRESS_IPV4,
      VIR_DOMAIN_EVENT_GRAPHICS_ADDRESS_IPV6,
      VIR_DOMAIN_EVENT_GRAPHICS_ADDRESS_UNIX);
   pragma Convention (C, virDomainEventGraphicsAddressType);  -- /usr/include/libvirt/libvirt-domain.h:3381

  --*
  -- * virDomainEventGraphicsAddress:
  -- *
  -- * The data structure containing connection address details
  -- *
  --  

  -- Address family, virDomainEventGraphicsAddressType  
   type u_virDomainEventGraphicsAddress is record
      family : aliased int;  -- /usr/include/libvirt/libvirt-domain.h:3391
      node : Interfaces.C.Strings.chars_ptr;  -- /usr/include/libvirt/libvirt-domain.h:3392
      service : Interfaces.C.Strings.chars_ptr;  -- /usr/include/libvirt/libvirt-domain.h:3393
   end record;
   pragma Convention (C_Pass_By_Copy, u_virDomainEventGraphicsAddress);  -- /usr/include/libvirt/libvirt-domain.h:3390

  -- Address of node (eg IP address, or UNIX path)  
  -- Service name/number (eg TCP port, or NULL)  
   subtype virDomainEventGraphicsAddress is u_virDomainEventGraphicsAddress;

   type virDomainEventGraphicsAddressPtr is access all virDomainEventGraphicsAddress;  -- /usr/include/libvirt/libvirt-domain.h:3396

  --*
  -- * virDomainEventGraphicsSubjectIdentity:
  -- *
  -- * The data structure representing a single identity
  -- *
  -- * The types of identity differ according to the authentication scheme,
  -- * some examples are 'x509dname' and 'saslUsername'.
  --  

  -- Type of identity  
   type u_virDomainEventGraphicsSubjectIdentity is record
      c_type : Interfaces.C.Strings.chars_ptr;  -- /usr/include/libvirt/libvirt-domain.h:3408
      name : Interfaces.C.Strings.chars_ptr;  -- /usr/include/libvirt/libvirt-domain.h:3409
   end record;
   pragma Convention (C_Pass_By_Copy, u_virDomainEventGraphicsSubjectIdentity);  -- /usr/include/libvirt/libvirt-domain.h:3407

  -- Identity value  
   subtype virDomainEventGraphicsSubjectIdentity is u_virDomainEventGraphicsSubjectIdentity;

   type virDomainEventGraphicsSubjectIdentityPtr is access all virDomainEventGraphicsSubjectIdentity;  -- /usr/include/libvirt/libvirt-domain.h:3412

  --*
  -- * virDomainEventGraphicsSubject:
  -- *
  -- * The data structure representing an authenticated subject
  -- *
  -- * A subject will have zero or more identities. The types of
  -- * identity differ according to the authentication scheme
  --  

  -- Number of identities in array 
   type u_virDomainEventGraphicsSubject is record
      nidentity : aliased int;  -- /usr/include/libvirt/libvirt-domain.h:3424
      identities : virDomainEventGraphicsSubjectIdentityPtr;  -- /usr/include/libvirt/libvirt-domain.h:3425
   end record;
   pragma Convention (C_Pass_By_Copy, u_virDomainEventGraphicsSubject);  -- /usr/include/libvirt/libvirt-domain.h:3423

  -- Array of identities for subject  
   subtype virDomainEventGraphicsSubject is u_virDomainEventGraphicsSubject;

   type virDomainEventGraphicsSubjectPtr is access all virDomainEventGraphicsSubject;  -- /usr/include/libvirt/libvirt-domain.h:3428

  --*
  -- * virConnectDomainEventGraphicsCallback:
  -- * @conn: connection object
  -- * @dom: domain on which the event occurred
  -- * @phase: the phase of the connection
  -- * @local: the local server address
  -- * @remote: the remote client address
  -- * @authScheme: the authentication scheme activated
  -- * @subject: the authenticated subject (user)
  -- * @opaque: application specified data
  -- *
  -- * The callback signature to use when registering for an event of type
  -- * VIR_DOMAIN_EVENT_ID_GRAPHICS with virConnectDomainEventRegisterAny()
  --  

   type virConnectDomainEventGraphicsCallback is access procedure
        (arg1 : VirConnectPtr;
         arg2 : virDomainPtr;
         arg3 : int;
         arg4 : System.Address;
         arg5 : System.Address;
         arg6 : Interfaces.C.Strings.chars_ptr;
         arg7 : System.Address;
         arg8 : System.Address);
   pragma Convention (C, virConnectDomainEventGraphicsCallback);  -- /usr/include/libvirt/libvirt-domain.h:3445

  --*
  -- * virConnectDomainEventBlockJobStatus:
  -- *
  -- * Tracks status of a virDomainBlockPull(), virDomainBlockRebase(),
  -- * virDomainBlockCopy(), or virDomainBlockCommit() operation
  --  

   type virConnectDomainEventBlockJobStatus is 
     (VIR_DOMAIN_BLOCK_JOB_COMPLETED,
      VIR_DOMAIN_BLOCK_JOB_FAILED,
      VIR_DOMAIN_BLOCK_JOB_CANCELED,
      VIR_DOMAIN_BLOCK_JOB_READY);
   pragma Convention (C, virConnectDomainEventBlockJobStatus);  -- /usr/include/libvirt/libvirt-domain.h:3469

  --*
  -- * virConnectDomainEventBlockJobCallback:
  -- * @conn: connection object
  -- * @dom: domain on which the event occurred
  -- * @disk: name associated with the affected disk (filename or target
  -- *        device, depending on how the callback was registered)
  -- * @type: type of block job (virDomainBlockJobType)
  -- * @status: status of the operation (virConnectDomainEventBlockJobStatus)
  -- * @opaque: application specified data
  -- *
  -- * The string returned for @disk can be used in any of the libvirt API
  -- * that operate on a particular disk of the domain, and depends on what
  -- * event type was registered with virConnectDomainEventRegisterAny().
  -- * If the callback was registered using the older type of
  -- * VIR_DOMAIN_EVENT_ID_BLOCK_JOB, then @disk contains the absolute file
  -- * name of the host resource for the active layer of the disk; however,
  -- * this name is unstable (pivoting via block copy or active block commit
  -- * will change which file is active, giving a different name for the two
  -- * events associated with the same job) and cannot be relied on if the
  -- * active layer is associated with a network resource.  If the callback
  -- * was registered using the newer type of VIR_DOMAIN_EVENT_ID_BLOCK_JOB_2,
  -- * then @disk will contain the device target shorthand (the <target
  -- * dev='...'/> sub-element, such as "vda").
  --  

   type virConnectDomainEventBlockJobCallback is access procedure
        (arg1 : VirConnectPtr;
         arg2 : virDomainPtr;
         arg3 : Interfaces.C.Strings.chars_ptr;
         arg4 : int;
         arg5 : int;
         arg6 : System.Address);
   pragma Convention (C, virConnectDomainEventBlockJobCallback);  -- /usr/include/libvirt/libvirt-domain.h:3495

  --*
  -- * virConnectDomainEventDiskChangeReason:
  -- *
  -- * The reason describing why this callback is called
  --  

  -- removable media changed to empty according to startup policy as source
  --     * was missing. oldSrcPath is set, newSrcPath is NULL  

  -- disk was dropped from domain as source file was missing.
  --     * oldSrcPath is set, newSrcPath is NULL  

   type virConnectDomainEventDiskChangeReason is 
     (VIR_DOMAIN_EVENT_DISK_CHANGE_MISSING_ON_START,
      VIR_DOMAIN_EVENT_DISK_DROP_MISSING_ON_START);
   pragma Convention (C, virConnectDomainEventDiskChangeReason);  -- /usr/include/libvirt/libvirt-domain.h:3518

  --*
  -- * virConnectDomainEventDiskChangeCallback:
  -- * @conn: connection object
  -- * @dom: domain on which the event occurred
  -- * @oldSrcPath: old source path
  -- * @newSrcPath: new source path
  -- * @devAlias: device alias name
  -- * @reason: reason why this callback was called; any of
  -- *          virConnectDomainEventDiskChangeReason
  -- * @opaque: application specified data
  -- *
  -- * This callback occurs when disk gets changed. However,
  -- * not all @reason will cause both @oldSrcPath and @newSrcPath
  -- * to be non-NULL. Please see virConnectDomainEventDiskChangeReason
  -- * for more details.
  -- *
  -- * The callback signature to use when registering for an event of type
  -- * VIR_DOMAIN_EVENT_ID_DISK_CHANGE with virConnectDomainEventRegisterAny()
  --  

   type virConnectDomainEventDiskChangeCallback is access procedure
        (arg1 : VirConnectPtr;
         arg2 : virDomainPtr;
         arg3 : Interfaces.C.Strings.chars_ptr;
         arg4 : Interfaces.C.Strings.chars_ptr;
         arg5 : Interfaces.C.Strings.chars_ptr;
         arg6 : int;
         arg7 : System.Address);
   pragma Convention (C, virConnectDomainEventDiskChangeCallback);  -- /usr/include/libvirt/libvirt-domain.h:3539

  --*
  -- * virConnectDomainEventTrayChangeReason:
  -- *
  -- * The reason describing why the callback was called
  --  

   type virDomainEventTrayChangeReason is 
     (VIR_DOMAIN_EVENT_TRAY_CHANGE_OPEN,
      VIR_DOMAIN_EVENT_TRAY_CHANGE_CLOSE);
   pragma Convention (C, virDomainEventTrayChangeReason);  -- /usr/include/libvirt/libvirt-domain.h:3559

  --*
  -- * virConnectDomainEventTrayChangeCallback:
  -- * @conn: connection object
  -- * @dom: domain on which the event occurred
  -- * @devAlias: device alias
  -- * @reason: why the tray status was changed?
  -- * @opaque: application specified data
  -- *
  -- * This callback occurs when the tray of a removable device is moved.
  -- *
  -- * The callback signature to use when registering for an event of type
  -- * VIR_DOMAIN_EVENT_ID_TRAY_CHANGE with virConnectDomainEventRegisterAny()
  --  

   type virConnectDomainEventTrayChangeCallback is access procedure
        (arg1 : VirConnectPtr;
         arg2 : virDomainPtr;
         arg3 : Interfaces.C.Strings.chars_ptr;
         arg4 : int;
         arg5 : System.Address);
   pragma Convention (C, virConnectDomainEventTrayChangeCallback);  -- /usr/include/libvirt/libvirt-domain.h:3574

  --*
  -- * virConnectDomainEventPMWakeupCallback:
  -- * @conn: connection object
  -- * @dom: domain on which the event occurred
  -- * @reason: reason why the callback was called, unused currently,
  -- *          always passes 0
  -- * @opaque: application specified data
  -- *
  -- * This callback occurs when the guest is woken up.
  -- *
  -- * The callback signature to use when registering for an event of type
  -- * VIR_DOMAIN_EVENT_ID_PMWAKEUP with virConnectDomainEventRegisterAny()
  --  

   type virConnectDomainEventPMWakeupCallback is access procedure
        (arg1 : VirConnectPtr;
         arg2 : virDomainPtr;
         arg3 : int;
         arg4 : System.Address);
   pragma Convention (C, virConnectDomainEventPMWakeupCallback);  -- /usr/include/libvirt/libvirt-domain.h:3593

  --*
  -- * virConnectDomainEventPMSuspendCallback:
  -- * @conn: connection object
  -- * @dom: domain on which the event occurred
  -- * @reason: reason why the callback was called, unused currently,
  -- *          always passes 0
  -- * @opaque: application specified data
  -- *
  -- * This callback occurs when the guest is suspended.
  -- *
  -- * The callback signature to use when registering for an event of type
  -- * VIR_DOMAIN_EVENT_ID_PMSUSPEND with virConnectDomainEventRegisterAny()
  --  

   type virConnectDomainEventPMSuspendCallback is access procedure
        (arg1 : VirConnectPtr;
         arg2 : virDomainPtr;
         arg3 : int;
         arg4 : System.Address);
   pragma Convention (C, virConnectDomainEventPMSuspendCallback);  -- /usr/include/libvirt/libvirt-domain.h:3611

  --*
  -- * virConnectDomainEventBalloonChangeCallback:
  -- * @conn: connection object
  -- * @dom: domain on which the event occurred
  -- * @actual: the new balloon level measured in kibibytes(blocks of 1024 bytes)
  -- * @opaque: application specified data
  -- *
  -- * The callback signature to use when registering for an event of type
  -- * VIR_DOMAIN_EVENT_ID_BALLOON_CHANGE with virConnectDomainEventRegisterAny()
  --  

   type virConnectDomainEventBalloonChangeCallback is access procedure
        (arg1 : VirConnectPtr;
         arg2 : virDomainPtr;
         arg3 : Extensions.unsigned_long_long;
         arg4 : System.Address);
   pragma Convention (C, virConnectDomainEventBalloonChangeCallback);  -- /usr/include/libvirt/libvirt-domain.h:3627

  --*
  -- * virConnectDomainEventPMSuspendDiskCallback:
  -- * @conn: connection object
  -- * @dom: domain on which the event occurred
  -- * @reason: reason why the callback was called, unused currently,
  -- *          always passes 0
  -- * @opaque: application specified data
  -- *
  -- * This callback occurs when the guest is suspended to disk.
  -- *
  -- * The callback signature to use when registering for an event of type
  -- * VIR_DOMAIN_EVENT_ID_PMSUSPEND_DISK with virConnectDomainEventRegisterAny()
  --  

   type virConnectDomainEventPMSuspendDiskCallback is access procedure
        (arg1 : VirConnectPtr;
         arg2 : virDomainPtr;
         arg3 : int;
         arg4 : System.Address);
   pragma Convention (C, virConnectDomainEventPMSuspendDiskCallback);  -- /usr/include/libvirt/libvirt-domain.h:3645

  --*
  -- * virConnectDomainEventDeviceRemovedCallback:
  -- * @conn: connection object
  -- * @dom: domain on which the event occurred
  -- * @devAlias: device alias
  -- * @opaque: application specified data
  -- *
  -- * This callback occurs when a device is removed from the domain.
  -- *
  -- * The callback signature to use when registering for an event of type
  -- * VIR_DOMAIN_EVENT_ID_DEVICE_REMOVED with virConnectDomainEventRegisterAny()
  --  

   type virConnectDomainEventDeviceRemovedCallback is access procedure
        (arg1 : VirConnectPtr;
         arg2 : virDomainPtr;
         arg3 : Interfaces.C.Strings.chars_ptr;
         arg4 : System.Address);
   pragma Convention (C, virConnectDomainEventDeviceRemovedCallback);  -- /usr/include/libvirt/libvirt-domain.h:3662

  --*
  -- * virConnectDomainEventDeviceAddedCallback:
  -- * @conn: connection object
  -- * @dom: domain on which the event occurred
  -- * @devAlias: device alias
  -- * @opaque: application specified data
  -- *
  -- * This callback occurs when a device is added to the domain.
  -- *
  -- * The callback signature to use when registering for an event of type
  -- * VIR_DOMAIN_EVENT_ID_DEVICE_ADDED with virConnectDomainEventRegisterAny()
  --  

   type virConnectDomainEventDeviceAddedCallback is access procedure
        (arg1 : VirConnectPtr;
         arg2 : virDomainPtr;
         arg3 : Interfaces.C.Strings.chars_ptr;
         arg4 : System.Address);
   pragma Convention (C, virConnectDomainEventDeviceAddedCallback);  -- /usr/include/libvirt/libvirt-domain.h:3679

  --*
  -- * virConnectDomainEventDeviceRemovalFailedCallback:
  -- * @conn: connection object
  -- * @dom: domain on which the event occurred
  -- * @devAlias: device alias
  -- * @opaque: application specified data
  -- *
  -- * This callback occurs when it's certain that removal of a device failed.
  -- *
  -- * The callback signature to use when registering for an event of type
  -- * VIR_DOMAIN_EVENT_ID_DEVICE_REMOVAL_FAILED with
  -- * virConnectDomainEventRegisterAny().
  --  

   type virConnectDomainEventDeviceRemovalFailedCallback is access procedure
        (arg1 : VirConnectPtr;
         arg2 : virDomainPtr;
         arg3 : Interfaces.C.Strings.chars_ptr;
         arg4 : System.Address);
   pragma Convention (C, virConnectDomainEventDeviceRemovalFailedCallback);  -- /usr/include/libvirt/libvirt-domain.h:3698

  --*
  -- * virConnectDomainEventMigrationIterationCallback:
  -- * @conn: connection object
  -- * @dom: domain on which the event occurred
  -- * @iteration: current iteration over domain's memory
  -- * @opaque: application specific data
  -- *
  -- * This callback occurs during live migration when a new iteration over
  -- * domain's memory starts. The @iteration value is increased by one every
  -- * time a new iteration is started to transfer memory pages dirtied since
  -- * the last iteration.
  -- *
  -- * The callback signature to use when registering for an event of type
  -- * VIR_DOMAIN_EVENT_ID_MIGRATION_ITERATION with
  -- * virConnectDomainEventRegisterAny().
  --  

   type virConnectDomainEventMigrationIterationCallback is access procedure
        (arg1 : VirConnectPtr;
         arg2 : virDomainPtr;
         arg3 : int;
         arg4 : System.Address);
   pragma Convention (C, virConnectDomainEventMigrationIterationCallback);  -- /usr/include/libvirt/libvirt-domain.h:3720

  --*
  -- * virConnectDomainEventJobCompletedCallback:
  -- * @conn: connection object
  -- * @dom: domain on which the event occurred
  -- * @params: job statistics stored as an array of virTypedParameter
  -- * @nparams: size of the params array
  -- * @opaque: application specific data
  -- *
  -- * This callback occurs when a job (such as migration) running on the domain
  -- * is completed. The params array will contain statistics of the just completed
  -- * job as virDomainGetJobStats would return. The callback must not free @params
  -- * (the array will be freed once the callback finishes).
  -- *
  -- * The callback signature to use when registering for an event of type
  -- * VIR_DOMAIN_EVENT_ID_JOB_COMPLETED with
  -- * virConnectDomainEventRegisterAny().
  --  

   type virConnectDomainEventJobCompletedCallback is access procedure
        (arg1 : VirConnectPtr;
         arg2 : virDomainPtr;
         arg3 : virTypedParameterPtr;
         arg4 : int;
         arg5 : System.Address);
   pragma Convention (C, virConnectDomainEventJobCompletedCallback);  -- /usr/include/libvirt/libvirt-domain.h:3742

  --*
  -- * VIR_DOMAIN_TUNABLE_CPU_VCPUPIN:
  -- *
  -- * Macro represents formatted pinning for one vcpu specified by id which is
  -- * appended to the parameter name, for example "cputune.vcpupin1",
  -- * as VIR_TYPED_PARAM_STRING.
  --  

  --*
  -- * VIR_DOMAIN_TUNABLE_CPU_EMULATORPIN:
  -- *
  -- * Macro represents formatted pinning for emulator process,
  -- * as VIR_TYPED_PARAM_STRING.
  --  

  --*
  -- * VIR_DOMAIN_TUNABLE_CPU_IOTHREADSPIN:
  -- *
  -- * Macro represents formatted pinning for one IOThread specified by id which is
  -- * appended to the parameter name, for example "cputune.iothreadpin1",
  -- * as VIR_TYPED_PARAM_STRING.
  --  

  --*
  -- * VIR_DOMAIN_TUNABLE_CPU_CPU_SHARES:
  -- *
  -- * Macro represents proportional weight of the scheduler used on the
  -- * host cpu, when using the posix scheduler, as VIR_TYPED_PARAM_ULLONG.
  --  

  --*
  -- * VIR_DOMAIN_TUNABLE_CPU_GLOBAL_PERIOD:
  -- *
  -- * Macro represents the enforcement period for a quota, in microseconds,
  -- * for whole domain, when using the posix scheduler, as VIR_TYPED_PARAM_ULLONG.
  --  

  --*
  -- * VIR_DOMAIN_TUNABLE_CPU_GLOBAL_QUOTA:
  -- *
  -- * Macro represents the maximum bandwidth to be used within a period for
  -- * whole domain, when using the posix scheduler, as VIR_TYPED_PARAM_LLONG.
  --  

  --*
  -- * VIR_DOMAIN_TUNABLE_CPU_VCPU_PERIOD:
  -- *
  -- * Macro represents the enforcement period for a quota, in microseconds,
  -- * for vcpus only, when using the posix scheduler, as VIR_TYPED_PARAM_ULLONG.
  --  

  --*
  -- * VIR_DOMAIN_TUNABLE_CPU_VCPU_QUOTA:
  -- *
  -- * Macro represents the maximum bandwidth to be used within a period for
  -- * vcpus only, when using the posix scheduler, as VIR_TYPED_PARAM_LLONG.
  --  

  --*
  -- * VIR_DOMAIN_TUNABLE_CPU_EMULATOR_PERIOD:
  -- *
  -- * Macro represents the enforcement period for a quota in microseconds,
  -- * when using the posix scheduler, for all emulator activity not tied to
  -- * vcpus, as VIR_TYPED_PARAM_ULLONG.
  --  

  --*
  -- * VIR_DOMAIN_TUNABLE_CPU_EMULATOR_QUOTA:
  -- *
  -- * Macro represents the maximum bandwidth to be used within a period for
  -- * all emulator activity not tied to vcpus, when using the posix scheduler,
  -- * as an VIR_TYPED_PARAM_LLONG.
  --  

  --*
  -- * VIR_DOMAIN_TUNABLE_CPU_IOTHREAD_PERIOD:
  -- *
  -- * Macro represents the enforcement period for a quota, in microseconds, for
  -- * iothreads only, when using the posix scheduler, as VIR_TYPED_PARAM_ULLONG.
  --  

  --*
  -- * VIR_DOMAIN_TUNABLE_CPU_IOTHREAD_QUOTA:
  -- *
  -- * Macro represents the maximum bandwidth to be used within a period for
  -- * iothreads only, when using the posix scheduler, as VIR_TYPED_PARAM_LLONG.
  --  

  --*
  -- * VIR_DOMAIN_TUNABLE_BLKDEV_DISK:
  -- *
  -- * Macro represents the name of guest disk for which the values are updated,
  -- * as VIR_TYPED_PARAM_STRING.
  --  

  --*
  -- * VIR_DOMAIN_TUNABLE_BLKDEV_TOTAL_BYTES_SEC:
  -- *
  -- * Macro represents the total throughput limit in bytes per second,
  -- * as VIR_TYPED_PARAM_ULLONG.
  --  

  --*
  -- * VIR_DOMAIN_TUNABLE_BLKDEV_READ_BYTES_SEC:
  -- *
  -- * Macro represents the read throughput limit in bytes per second,
  -- * as VIR_TYPED_PARAM_ULLONG.
  --  

  --*
  -- * VIR_DOMAIN_TUNABLE_BLKDEV_WRITE_BYTES_SEC:
  -- *
  -- * Macro represents the write throughput limit in bytes per second,
  -- * as VIR_TYPED_PARAM_ULLONG.
  --  

  --*
  -- * VIR_DOMAIN_TUNABLE_BLKDEV_TOTAL_IOPS_SEC:
  -- *
  -- * Macro represents the total I/O operations per second,
  -- * as VIR_TYPED_PARAM_ULLONG.
  --  

  --*
  -- * VIR_DOMAIN_TUNABLE_BLKDEV_READ_IOPS_SEC:
  -- *
  -- * Macro represents the read I/O operations per second,
  -- * as VIR_TYPED_PARAM_ULLONG.
  --  

  --*
  -- * VIR_DOMAIN_TUNABLE_BLKDEV_WRITE_IOPS_SEC:
  -- *
  -- * Macro represents the write I/O operations per second,
  -- * as VIR_TYPED_PARAM_ULLONG.
  --  

  --*
  -- * VIR_DOMAIN_TUNABLE_BLKDEV_TOTAL_BYTES_SEC_MAX:
  -- *
  -- * Macro represents the total throughput limit during bursts in
  -- * maximum bytes per second, as VIR_TYPED_PARAM_ULLONG.
  --  

  --*
  -- * VIR_DOMAIN_TUNABLE_BLKDEV_READ_BYTES_SEC_MAX:
  -- *
  -- * Macro represents the read throughput limit during bursts in
  -- * maximum bytes per second, as VIR_TYPED_PARAM_ULLONG.
  --  

  --*
  -- * VIR_DOMAIN_TUNABLE_BLKDEV_WRITE_BYTES_SEC_MAX:
  -- *
  -- * Macro represents the write throughput limit during bursts in
  -- * maximum bytes per second, as VIR_TYPED_PARAM_ULLONG.
  --  

  --*
  -- * VIR_DOMAIN_TUNABLE_BLKDEV_TOTAL_IOPS_SEC_MAX:
  -- *
  -- * Macro represents the total maximum I/O operations per second during bursts,
  -- * as VIR_TYPED_PARAM_ULLONG.
  --  

  --*
  -- * VIR_DOMAIN_TUNABLE_BLKDEV_READ_IOPS_SEC_MAX:
  -- *
  -- * Macro represents the read maximum I/O operations per second during bursts,
  -- * as VIR_TYPED_PARAM_ULLONG.
  --  

  --*
  -- * VIR_DOMAIN_TUNABLE_BLKDEV_WRITE_IOPS_SEC_MAX:
  -- *
  -- * Macro represents the write maximum I/O operations per second during bursts,
  -- * as VIR_TYPED_PARAM_ULLONG.
  --  

  --*
  -- * VIR_DOMAIN_TUNABLE_BLKDEV_SIZE_IOPS_SEC:
  -- *
  -- * Macro represents the size maximum I/O operations per second,
  -- * as VIR_TYPED_PARAM_ULLONG.
  --  

  --*
  -- * VIR_DOMAIN_TUNABLE_BLKDEV_TOTAL_BYTES_SEC_MAX_LENGTH:
  -- *
  -- * Macro represents the length in seconds allowed for a burst period
  -- * for the blkdeviotune.total_bytes_sec_max,
  -- * as VIR_TYPED_PARAM_ULLONG.
  --  

  --*
  -- * VIR_DOMAIN_TUNABLE_BLKDEV_READ_BYTES_SEC_MAX_LENGTH:
  -- *
  -- * Macro represents the length in seconds allowed for a burst period
  -- * for the blkdeviotune.read_bytes_sec_max
  -- * as VIR_TYPED_PARAM_ULLONG.
  --  

  --*
  -- * VIR_DOMAIN_TUNABLE_BLKDEV_WRITE_BYTES_SEC_MAX_LENGTH:
  -- *
  -- * Macro represents the length in seconds allowed for a burst period
  -- * for the blkdeviotune.write_bytes_sec_max
  -- * as VIR_TYPED_PARAM_ULLONG.
  --  

  --*
  -- * VIR_DOMAIN_TUNABLE_BLKDEV_TOTAL_IOPS_SEC_MAX_LENGTH:
  -- *
  -- * Macro represents the length in seconds allowed for a burst period
  -- * for the blkdeviotune.total_iops_sec_max
  -- * as VIR_TYPED_PARAM_ULLONG.
  --  

  --*
  -- * VIR_DOMAIN_TUNABLE_BLKDEV_READ_IOPS_SEC_MAX_LENGTH:
  -- *
  -- * Macro represents the length in seconds allowed for a burst period
  -- * for the blkdeviotune.read_iops_sec_max
  -- * as VIR_TYPED_PARAM_ULLONG.
  --  

  --*
  -- * VIR_DOMAIN_TUNABLE_BLKDEV_WRITE_IOPS_SEC_MAX_LENGTH:
  -- *
  -- * Macro represents the length in seconds allowed for a burst period
  -- * for the blkdeviotune.write_iops_sec_max
  -- * as VIR_TYPED_PARAM_ULLONG.
  --  

  --*
  -- * virConnectDomainEventTunableCallback:
  -- * @conn: connection object
  -- * @dom: domain on which the event occurred
  -- * @params: changed tunable values stored as array of virTypedParameter
  -- * @nparams: size of the array
  -- * @opaque: application specified data
  -- *
  -- * This callback occurs when tunable values are updated. The params must not
  -- * be freed in the callback handler as it's done internally after the callback
  -- * handler is executed.
  -- *
  -- * Currently supported name spaces:
  -- *  "cputune.*"
  -- *  "blkdeviotune.*"
  -- *
  -- * The callback signature to use when registering for an event of type
  -- * VIR_DOMAIN_EVENT_ID_TUNABLE with virConnectDomainEventRegisterAny()
  --  

   type virConnectDomainEventTunableCallback is access procedure
        (arg1 : VirConnectPtr;
         arg2 : virDomainPtr;
         arg3 : virTypedParameterPtr;
         arg4 : int;
         arg5 : System.Address);
   pragma Convention (C, virConnectDomainEventTunableCallback);  -- /usr/include/libvirt/libvirt-domain.h:4033

  -- agent connected  
  -- agent disconnected  
   subtype virConnectDomainEventAgentLifecycleState is unsigned;
   VIR_CONNECT_DOMAIN_EVENT_AGENT_LIFECYCLE_STATE_CONNECTED : constant virConnectDomainEventAgentLifecycleState := 1;
   VIR_CONNECT_DOMAIN_EVENT_AGENT_LIFECYCLE_STATE_DISCONNECTED : constant virConnectDomainEventAgentLifecycleState := 2;  -- /usr/include/libvirt/libvirt-domain.h:4047

  -- unknown state change reason  
  -- state changed due to domain start  
  -- channel state changed  
   type virConnectDomainEventAgentLifecycleReason is 
     (VIR_CONNECT_DOMAIN_EVENT_AGENT_LIFECYCLE_REASON_UNKNOWN,
      VIR_CONNECT_DOMAIN_EVENT_AGENT_LIFECYCLE_REASON_DOMAIN_STARTED,
      VIR_CONNECT_DOMAIN_EVENT_AGENT_LIFECYCLE_REASON_CHANNEL);
   pragma Convention (C, virConnectDomainEventAgentLifecycleReason);  -- /usr/include/libvirt/libvirt-domain.h:4057

  --*
  -- * virConnectDomainEventAgentLifecycleCallback:
  -- * @conn: connection object
  -- * @dom: domain on which the event occurred
  -- * @state: new state of the guest agent, one of virConnectDomainEventAgentLifecycleState
  -- * @reason: reason for state change; one of virConnectDomainEventAgentLifecycleReason
  -- * @opaque: application specified data
  -- *
  -- * This callback occurs when libvirt detects a change in the state of a guest
  -- * agent.
  -- *
  -- * The callback signature to use when registering for an event of type
  -- * VIR_DOMAIN_EVENT_ID_AGENT_LIFECYCLE with virConnectDomainEventRegisterAny()
  --  

   type virConnectDomainEventAgentLifecycleCallback is access procedure
        (arg1 : VirConnectPtr;
         arg2 : virDomainPtr;
         arg3 : int;
         arg4 : int;
         arg5 : System.Address);
   pragma Convention (C, virConnectDomainEventAgentLifecycleCallback);  -- /usr/include/libvirt/libvirt-domain.h:4073

  --*
  -- * VIR_DOMAIN_EVENT_CALLBACK:
  -- *
  -- * Used to cast the event specific callback into the generic one
  -- * for use for virConnectDomainEventRegisterAny()
  --  

  --*
  -- * virDomainEventID:
  -- *
  -- * An enumeration of supported eventId parameters for
  -- * virConnectDomainEventRegisterAny().  Each event id determines which
  -- * signature of callback function will be used.
  --  

  -- virConnectDomainEventCallback  
  -- virConnectDomainEventGenericCallback  
  -- virConnectDomainEventRTCChangeCallback  
  -- virConnectDomainEventWatchdogCallback  
  -- virConnectDomainEventIOErrorCallback  
  -- virConnectDomainEventGraphicsCallback  
  -- virConnectDomainEventIOErrorReasonCallback  
  -- virConnectDomainEventGenericCallback  
  -- virConnectDomainEventBlockJobCallback  
  -- virConnectDomainEventDiskChangeCallback  
  -- virConnectDomainEventTrayChangeCallback  
  -- virConnectDomainEventPMWakeupCallback  
  -- virConnectDomainEventPMSuspendCallback  
  -- virConnectDomainEventBalloonChangeCallback  
  -- virConnectDomainEventPMSuspendDiskCallback  
  -- virConnectDomainEventDeviceRemovedCallback  
  -- virConnectDomainEventBlockJobCallback  
  -- virConnectDomainEventTunableCallback  
  -- virConnectDomainEventAgentLifecycleCallback  
  -- virConnectDomainEventDeviceAddedCallback  
  -- virConnectDomainEventMigrationIterationCallback  
  -- virConnectDomainEventJobCompletedCallback  
  -- virConnectDomainEventDeviceRemovalFailedCallback  
  --     * NB: this enum value will increase over time as new events are
  --     * added to the libvirt API. It reflects the last event ID supported
  --     * by this version of the libvirt API.
  --      

   type virDomainEventID is 
     (VIR_DOMAIN_EVENT_ID_LIFECYCLE,
      VIR_DOMAIN_EVENT_ID_REBOOT,
      VIR_DOMAIN_EVENT_ID_RTC_CHANGE,
      VIR_DOMAIN_EVENT_ID_WATCHDOG,
      VIR_DOMAIN_EVENT_ID_IO_ERROR,
      VIR_DOMAIN_EVENT_ID_GRAPHICS,
      VIR_DOMAIN_EVENT_ID_IO_ERROR_REASON,
      VIR_DOMAIN_EVENT_ID_CONTROL_ERROR,
      VIR_DOMAIN_EVENT_ID_BLOCK_JOB,
      VIR_DOMAIN_EVENT_ID_DISK_CHANGE,
      VIR_DOMAIN_EVENT_ID_TRAY_CHANGE,
      VIR_DOMAIN_EVENT_ID_PMWAKEUP,
      VIR_DOMAIN_EVENT_ID_PMSUSPEND,
      VIR_DOMAIN_EVENT_ID_BALLOON_CHANGE,
      VIR_DOMAIN_EVENT_ID_PMSUSPEND_DISK,
      VIR_DOMAIN_EVENT_ID_DEVICE_REMOVED,
      VIR_DOMAIN_EVENT_ID_BLOCK_JOB_2,
      VIR_DOMAIN_EVENT_ID_TUNABLE,
      VIR_DOMAIN_EVENT_ID_AGENT_LIFECYCLE,
      VIR_DOMAIN_EVENT_ID_DEVICE_ADDED,
      VIR_DOMAIN_EVENT_ID_MIGRATION_ITERATION,
      VIR_DOMAIN_EVENT_ID_JOB_COMPLETED,
      VIR_DOMAIN_EVENT_ID_DEVICE_REMOVAL_FAILED);
   pragma Convention (C, virDomainEventID);  -- /usr/include/libvirt/libvirt-domain.h:4129

  -- Use VIR_DOMAIN_EVENT_CALLBACK() to cast the 'cb' parameter   
   function virConnectDomainEventRegisterAny
     (conn : VirConnectPtr;
      dom : virDomainPtr;
      eventID : int;
      cb : virConnectDomainEventGenericCallback;
      opaque : System.Address;
      freecb : virFreeCallback) return int;  -- /usr/include/libvirt/libvirt-domain.h:4133
   pragma Import (C, virConnectDomainEventRegisterAny, "virConnectDomainEventRegisterAny");

  -- Optional, to filter  
   function virConnectDomainEventDeregisterAny (conn : VirConnectPtr; callbackID : int) return int;  -- /usr/include/libvirt/libvirt-domain.h:4140
   pragma Import (C, virConnectDomainEventDeregisterAny, "virConnectDomainEventDeregisterAny");

  --*
  -- * virDomainConsoleFlags
  -- *
  -- * Since 0.9.10
  --  

  -- abort a (possibly) active console
  --                                            connection to force a new
  --                                            connection  

  -- check if the console driver supports
  --                                           safe console operations  

   subtype virDomainConsoleFlags is unsigned;
   VIR_DOMAIN_CONSOLE_FORCE : constant virDomainConsoleFlags := 1;
   VIR_DOMAIN_CONSOLE_SAFE : constant virDomainConsoleFlags := 2;  -- /usr/include/libvirt/libvirt-domain.h:4156

   function virDomainOpenConsole
     (dom : virDomainPtr;
      dev_name : Interfaces.C.Strings.chars_ptr;
      st : VirStreamPtr;
      flags : unsigned) return int;  -- /usr/include/libvirt/libvirt-domain.h:4158
   pragma Import (C, virDomainOpenConsole, "virDomainOpenConsole");

  --*
  -- * virDomainChannelFlags
  -- *
  -- * Since 1.0.2
  --  

  -- abort a (possibly) active channel
  --                                            connection to force a new
  --                                            connection  

   subtype virDomainChannelFlags is unsigned;
   VIR_DOMAIN_CHANNEL_FORCE : constant virDomainChannelFlags := 1;  -- /usr/include/libvirt/libvirt-domain.h:4172

   function virDomainOpenChannel
     (dom : virDomainPtr;
      name : Interfaces.C.Strings.chars_ptr;
      st : VirStreamPtr;
      flags : unsigned) return int;  -- /usr/include/libvirt/libvirt-domain.h:4174
   pragma Import (C, virDomainOpenChannel, "virDomainOpenChannel");

   subtype virDomainOpenGraphicsFlags is unsigned;
   VIR_DOMAIN_OPEN_GRAPHICS_SKIPAUTH : constant virDomainOpenGraphicsFlags := 1;  -- /usr/include/libvirt/libvirt-domain.h:4181

   function virDomainOpenGraphics
     (dom : virDomainPtr;
      idx : unsigned;
      fd : int;
      flags : unsigned) return int;  -- /usr/include/libvirt/libvirt-domain.h:4183
   pragma Import (C, virDomainOpenGraphics, "virDomainOpenGraphics");

   function virDomainOpenGraphicsFD
     (dom : virDomainPtr;
      idx : unsigned;
      flags : unsigned) return int;  -- /usr/include/libvirt/libvirt-domain.h:4188
   pragma Import (C, virDomainOpenGraphicsFD, "virDomainOpenGraphicsFD");

   function virDomainInjectNMI (domain : virDomainPtr; flags : unsigned) return int;  -- /usr/include/libvirt/libvirt-domain.h:4192
   pragma Import (C, virDomainInjectNMI, "virDomainInjectNMI");

   function virDomainFSTrim
     (dom : virDomainPtr;
      mountPoint : Interfaces.C.Strings.chars_ptr;
      minimum : Extensions.unsigned_long_long;
      flags : unsigned) return int;  -- /usr/include/libvirt/libvirt-domain.h:4194
   pragma Import (C, virDomainFSTrim, "virDomainFSTrim");

   function virDomainFSFreeze
     (dom : virDomainPtr;
      mountpoints : System.Address;
      nmountpoints : unsigned;
      flags : unsigned) return int;  -- /usr/include/libvirt/libvirt-domain.h:4199
   pragma Import (C, virDomainFSFreeze, "virDomainFSFreeze");

   function virDomainFSThaw
     (dom : virDomainPtr;
      mountpoints : System.Address;
      nmountpoints : unsigned;
      flags : unsigned) return int;  -- /usr/include/libvirt/libvirt-domain.h:4204
   pragma Import (C, virDomainFSThaw, "virDomainFSThaw");

  --*
  -- * virDomainFSInfo:
  -- *
  -- * The data structure containing mounted file systems within a guset
  -- *
  --  

   type u_virDomainFSInfo;
   subtype virDomainFSInfo is u_virDomainFSInfo;

   type virDomainFSInfoPtr is new System.Address;  -- /usr/include/libvirt/libvirt-domain.h:4216

  -- path to mount point  
   type u_virDomainFSInfo is record
      mountpoint : Interfaces.C.Strings.chars_ptr;  -- /usr/include/libvirt/libvirt-domain.h:4218
      name : Interfaces.C.Strings.chars_ptr;  -- /usr/include/libvirt/libvirt-domain.h:4219
      fstype : Interfaces.C.Strings.chars_ptr;  -- /usr/include/libvirt/libvirt-domain.h:4220
      ndevAlias : aliased size_t;  -- /usr/include/libvirt/libvirt-domain.h:4221
      devAlias : System.Address;  -- /usr/include/libvirt/libvirt-domain.h:4222
   end record;
   pragma Convention (C_Pass_By_Copy, u_virDomainFSInfo);  -- /usr/include/libvirt/libvirt-domain.h:4217

  -- device name in the guest (e.g. "sda1")  
  -- filesystem type  
  -- number of elements in devAlias  
  -- array of disk device aliases  
   procedure virDomainFSInfoFree (info : virDomainFSInfoPtr);  -- /usr/include/libvirt/libvirt-domain.h:4225
   pragma Import (C, virDomainFSInfoFree, "virDomainFSInfoFree");

   function virDomainGetFSInfo
     (dom : virDomainPtr;
      info : System.Address;
      flags : unsigned) return int;  -- /usr/include/libvirt/libvirt-domain.h:4227
   pragma Import (C, virDomainGetFSInfo, "virDomainGetFSInfo");

   function virDomainGetTime
     (dom : virDomainPtr;
      seconds : access Long_Long_Integer;
      nseconds : access unsigned;
      flags : unsigned) return int;  -- /usr/include/libvirt/libvirt-domain.h:4231
   pragma Import (C, virDomainGetTime, "virDomainGetTime");

  -- Re-sync domain time from domain's RTC  
   subtype virDomainSetTimeFlags is unsigned;
   VIR_DOMAIN_TIME_SYNC : constant virDomainSetTimeFlags := 1;  -- /usr/include/libvirt/libvirt-domain.h:4238

   function virDomainSetTime
     (dom : virDomainPtr;
      seconds : Long_Long_Integer;
      nseconds : unsigned;
      flags : unsigned) return int;  -- /usr/include/libvirt/libvirt-domain.h:4240
   pragma Import (C, virDomainSetTime, "virDomainSetTime");

  --*
  -- * virSchedParameterType:
  -- *
  -- * A scheduler parameter field type.  Provided for backwards
  -- * compatibility; virTypedParameterType is the preferred enum since
  -- * 0.9.2.
  --  

   subtype virSchedParameterType is unsigned;
   VIR_DOMAIN_SCHED_FIELD_INT : constant virSchedParameterType := 1;
   VIR_DOMAIN_SCHED_FIELD_UINT : constant virSchedParameterType := 2;
   VIR_DOMAIN_SCHED_FIELD_LLONG : constant virSchedParameterType := 3;
   VIR_DOMAIN_SCHED_FIELD_ULLONG : constant virSchedParameterType := 4;
   VIR_DOMAIN_SCHED_FIELD_DOUBLE : constant virSchedParameterType := 5;
   VIR_DOMAIN_SCHED_FIELD_BOOLEAN : constant virSchedParameterType := 6;  -- /usr/include/libvirt/libvirt-domain.h:4259

  --*
  -- * VIR_DOMAIN_SCHED_FIELD_LENGTH:
  -- *
  -- * Macro providing the field length of virSchedParameter.  Provided
  -- * for backwards compatibility; VIR_TYPED_PARAM_FIELD_LENGTH is the
  -- * preferred value since 0.9.2.
  --  

  --*
  -- * virSchedParameter:
  -- *
  -- * a virSchedParameter is the set of scheduler parameters.
  -- * Provided for backwards compatibility; virTypedParameter is the
  -- * preferred alias since 0.9.2.
  --  

   subtype virSchedParameter is u_virTypedParameter;

  --*
  -- * virSchedParameterPtr:
  -- *
  -- * a virSchedParameterPtr is a pointer to a virSchedParameter structure.
  -- * Provided for backwards compatibility; virTypedParameterPtr is the
  -- * preferred alias since 0.9.2.
  --  

   type virSchedParameterPtr is access all virSchedParameter;  -- /usr/include/libvirt/libvirt-domain.h:4287

  --*
  -- * virBlkioParameterType:
  -- *
  -- * A blkio parameter field type.  Provided for backwards
  -- * compatibility; virTypedParameterType is the preferred enum since
  -- * 0.9.2.
  --  

   subtype virBlkioParameterType is unsigned;
   VIR_DOMAIN_BLKIO_PARAM_INT : constant virBlkioParameterType := 1;
   VIR_DOMAIN_BLKIO_PARAM_UINT : constant virBlkioParameterType := 2;
   VIR_DOMAIN_BLKIO_PARAM_LLONG : constant virBlkioParameterType := 3;
   VIR_DOMAIN_BLKIO_PARAM_ULLONG : constant virBlkioParameterType := 4;
   VIR_DOMAIN_BLKIO_PARAM_DOUBLE : constant virBlkioParameterType := 5;
   VIR_DOMAIN_BLKIO_PARAM_BOOLEAN : constant virBlkioParameterType := 6;  -- /usr/include/libvirt/libvirt-domain.h:4303

  --*
  -- * VIR_DOMAIN_BLKIO_FIELD_LENGTH:
  -- *
  -- * Macro providing the field length of virBlkioParameter.  Provided
  -- * for backwards compatibility; VIR_TYPED_PARAM_FIELD_LENGTH is the
  -- * preferred value since 0.9.2.
  --  

  --*
  -- * virBlkioParameter:
  -- *
  -- * a virBlkioParameter is the set of blkio parameters.
  -- * Provided for backwards compatibility; virTypedParameter is the
  -- * preferred alias since 0.9.2.
  --  

   subtype virBlkioParameter is u_virTypedParameter;

  --*
  -- * virBlkioParameterPtr:
  -- *
  -- * a virBlkioParameterPtr is a pointer to a virBlkioParameter structure.
  -- * Provided for backwards compatibility; virTypedParameterPtr is the
  -- * preferred alias since 0.9.2.
  --  

   type virBlkioParameterPtr is access all virBlkioParameter;  -- /usr/include/libvirt/libvirt-domain.h:4331

  --*
  -- * virMemoryParameterType:
  -- *
  -- * A memory parameter field type.  Provided for backwards
  -- * compatibility; virTypedParameterType is the preferred enum since
  -- * 0.9.2.
  --  

   subtype virMemoryParameterType is unsigned;
   VIR_DOMAIN_MEMORY_PARAM_INT : constant virMemoryParameterType := 1;
   VIR_DOMAIN_MEMORY_PARAM_UINT : constant virMemoryParameterType := 2;
   VIR_DOMAIN_MEMORY_PARAM_LLONG : constant virMemoryParameterType := 3;
   VIR_DOMAIN_MEMORY_PARAM_ULLONG : constant virMemoryParameterType := 4;
   VIR_DOMAIN_MEMORY_PARAM_DOUBLE : constant virMemoryParameterType := 5;
   VIR_DOMAIN_MEMORY_PARAM_BOOLEAN : constant virMemoryParameterType := 6;  -- /usr/include/libvirt/libvirt-domain.h:4347

  --*
  -- * VIR_DOMAIN_MEMORY_FIELD_LENGTH:
  -- *
  -- * Macro providing the field length of virMemoryParameter.  Provided
  -- * for backwards compatibility; VIR_TYPED_PARAM_FIELD_LENGTH is the
  -- * preferred value since 0.9.2.
  --  

  --*
  -- * virMemoryParameter:
  -- *
  -- * a virMemoryParameter is the set of scheduler parameters.
  -- * Provided for backwards compatibility; virTypedParameter is the
  -- * preferred alias since 0.9.2.
  --  

   subtype virMemoryParameter is u_virTypedParameter;

  --*
  -- * virMemoryParameterPtr:
  -- *
  -- * a virMemoryParameterPtr is a pointer to a virMemoryParameter structure.
  -- * Provided for backwards compatibility; virTypedParameterPtr is the
  -- * preferred alias since 0.9.2.
  --  

   type virMemoryParameterPtr is access all virMemoryParameter;  -- /usr/include/libvirt/libvirt-domain.h:4375

  -- Parse DHCP lease file  
  -- Query qemu guest agent  
   type virDomainInterfaceAddressesSource is 
     (VIR_DOMAIN_INTERFACE_ADDRESSES_SRC_LEASE,
      VIR_DOMAIN_INTERFACE_ADDRESSES_SRC_AGENT);
   pragma Convention (C, virDomainInterfaceAddressesSource);  -- /usr/include/libvirt/libvirt-domain.h:4384

   type u_virDomainInterfaceIPAddress;
   subtype virDomainIPAddress is u_virDomainInterfaceIPAddress;

   type virDomainIPAddressPtr is new System.Address;  -- /usr/include/libvirt/libvirt-domain.h:4387

  -- virIPAddrType  
   type u_virDomainInterfaceIPAddress is record
      c_type : aliased int;  -- /usr/include/libvirt/libvirt-domain.h:4389
      addr : Interfaces.C.Strings.chars_ptr;  -- /usr/include/libvirt/libvirt-domain.h:4390
      prefix : aliased unsigned;  -- /usr/include/libvirt/libvirt-domain.h:4391
   end record;
   pragma Convention (C_Pass_By_Copy, u_virDomainInterfaceIPAddress);  -- /usr/include/libvirt/libvirt-domain.h:4388

  -- IP address  
  -- IP address prefix  
   type u_virDomainInterface;
   subtype virDomainInterface is u_virDomainInterface;

   type virDomainInterfacePtr is new System.Address;  -- /usr/include/libvirt/libvirt-domain.h:4395

  -- interface name  
   type u_virDomainInterface is record
      name : Interfaces.C.Strings.chars_ptr;  -- /usr/include/libvirt/libvirt-domain.h:4397
      hwaddr : Interfaces.C.Strings.chars_ptr;  -- /usr/include/libvirt/libvirt-domain.h:4398
      naddrs : aliased unsigned;  -- /usr/include/libvirt/libvirt-domain.h:4399
      addrs : virDomainIPAddressPtr;  -- /usr/include/libvirt/libvirt-domain.h:4400
   end record;
   pragma Convention (C_Pass_By_Copy, u_virDomainInterface);  -- /usr/include/libvirt/libvirt-domain.h:4396

  -- hardware address, may be NULL  
  -- number of items in @addrs  
  -- array of IP addresses  
   function virDomainInterfaceAddresses
     (dom : virDomainPtr;
      ifaces : System.Address;
      source : unsigned;
      flags : unsigned) return int;  -- /usr/include/libvirt/libvirt-domain.h:4403
   pragma Import (C, virDomainInterfaceAddresses, "virDomainInterfaceAddresses");

   procedure virDomainInterfaceFree (iface : virDomainInterfacePtr);  -- /usr/include/libvirt/libvirt-domain.h:4408
   pragma Import (C, virDomainInterfaceFree, "virDomainInterfaceFree");

  -- the password is already encrypted  
   subtype virDomainSetUserPasswordFlags is unsigned;
   VIR_DOMAIN_PASSWORD_ENCRYPTED : constant virDomainSetUserPasswordFlags := 1;  -- /usr/include/libvirt/libvirt-domain.h:4412

   function virDomainSetUserPassword
     (dom : virDomainPtr;
      user : Interfaces.C.Strings.chars_ptr;
      password : Interfaces.C.Strings.chars_ptr;
      flags : unsigned) return int;  -- /usr/include/libvirt/libvirt-domain.h:4414
   pragma Import (C, virDomainSetUserPassword, "virDomainSetUserPassword");

   function virDomainRename
     (dom : virDomainPtr;
      new_name : Interfaces.C.Strings.chars_ptr;
      flags : unsigned) return int;  -- /usr/include/libvirt/libvirt-domain.h:4419
   pragma Import (C, virDomainRename, "virDomainRename");

   function virDomainGetGuestVcpus
     (domain : virDomainPtr;
      params : System.Address;
      nparams : access unsigned;
      flags : unsigned) return int;  -- /usr/include/libvirt/libvirt-domain.h:4423
   pragma Import (C, virDomainGetGuestVcpus, "virDomainGetGuestVcpus");

   function virDomainSetGuestVcpus
     (domain : virDomainPtr;
      cpumap : Interfaces.C.Strings.chars_ptr;
      state : int;
      flags : unsigned) return int;  -- /usr/include/libvirt/libvirt-domain.h:4428
   pragma Import (C, virDomainSetGuestVcpus, "virDomainSetGuestVcpus");

end Libvirt_Domain_Api;
