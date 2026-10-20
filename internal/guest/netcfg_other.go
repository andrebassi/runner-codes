//go:build !linux

package guest

// configureLinkOS fora do Linux só recusa: o guest real é sempre Linux, e os testes no
// host trocam configureLink por um falso.
func configureLinkOS(NetConfig) error { return errNoNetSupport }
