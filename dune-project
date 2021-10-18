(lang dune 2.9)
(using dune_site 0.1)
(name conan)
(cram enable)

(package (name conan))
(package (name conan-database))
(package (name conan-unix))
(package (name conan-lwt))

(package
 (name conan-cli)
 (sites (share database)))

