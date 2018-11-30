("cd" to the directory containing this read-me file

To build the application, run the Maven command:

   mvn clean install

(or "mvn clean package", etc.).


To run the application, run the command

  scala -cp ./target/wm-1.0-SNAPSHOT.jar  com.us.dsb.wm.DroneScheduler ./src/test/scala/com/us/dsb/wm/orders1.dat

with the last command argument changed as needed to name another orders input
file.

The program writes a new schedule file in the temporary directory (per
File.createTemporaryFile(String, String), and writes the pathname of that
file to the standard output.
