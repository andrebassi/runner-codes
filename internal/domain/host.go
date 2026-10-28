package domain

// Host é o que o host do rc tem para máquinas novas (GET /api/v1/host): quem distribui máquinas entre
// vários rc (o HQ do superbot) escolhe por estes números em vez de adivinhar. Memória zero quer dizer
// "não medida" (fora do Linux não há /proc/meminfo): quem decide trata zero como desconhecido, não
// como cheio.
type Host struct {
	MemTotalMiB     int64 `json:"mem_total_mib"`
	MemAvailableMiB int64 `json:"mem_available_mib"`
	// Disco do diretório de estado do rc (onde moram os discos das máquinas).
	DiskTotalMiB int64 `json:"disk_total_mib"`
	DiskFreeMiB  int64 `json:"disk_free_mib"`
	// KVM diz se /dev/kvm existe: sem ele nenhuma máquina liga, por mais livre que o resto esteja.
	KVM      bool `json:"kvm"`
	Machines int  `json:"machines"`
	Running  int  `json:"running"`
	// CPU do host: núcleos, carga de 1 min e os ticks acumulados desde o boot (linha "cpu" do
	// /proc/stat). Acumulados, não porcentagem: o rc não guarda estado entre chamadas, e quem lê duas
	// vezes tira a % pela diferença (ocupado/total no intervalo). Zero = não medido.
	Cores         int     `json:"cores"`
	Load1         float64 `json:"load1"`
	CPUBusyTicks  uint64  `json:"cpu_busy_ticks"`
	CPUTotalTicks uint64  `json:"cpu_total_ticks"`
	// Procs é o consumo de cada máquina ligada no host (o processo do vmm dela).
	Procs []MachineProc `json:"procs,omitempty"`
	// PersistentDisks diz se o diretório de cada máquina mora num disco de rede só dela (rc serve
	// --persistent-disks gce): aí a máquina sobrevive ao host e muda de host soltando e adotando o disco, sem cópia.
	PersistentDisks bool `json:"persistent_disks"`
	// Preempting diz que o provedor avisou que vai tirar este host (spot: ~30 s de aviso). O rc já está desligando
	// as máquinas e soltando os discos; nenhuma máquina nova deve vir para cá. Drained são as que estavam ligadas no
	// aviso — quem orquestra religa essas em outro host.
	Preempting bool     `json:"preempting"`
	Drained    []string `json:"drained,omitempty"`
}

// MachineProc é o consumo do processo de uma máquina ligada: CPU acumulada (utime+stime, em
// nanossegundos, também pela diferença entre leituras) e memória residente no host.
type MachineProc struct {
	Name     string `json:"name"`
	PID      int    `json:"pid"`
	CPUNanos int64  `json:"cpu_nanos"`
	RSSBytes int64  `json:"rss_bytes"`
}
