Printexc.record_backtrace(true);

let g = Tectonic.run_phase(3, 3) |> Continent.run_phase;
Print.print(g, Continent.show);