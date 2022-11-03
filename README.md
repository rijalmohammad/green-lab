#  Energy Efficiency and Runtime Performance of Web Assembly Binaries

This is repository of Green Lab project about Evaluating the Energy Efficiency and Runtime Performance of Web Assembly Binaries of Different Programming Languages on Web Applications in Android Mobile Devices

## About Project

This project aims is to analyze and evaluate the impact of web assembly binaries compiled from different programming languages and running in different browsers on energy efficiency and runtime performance of Android mobile web applications.

Benchmark functions are considered as the subjects in the experiment. Six benchmark functions are selected from [Computer Language Benchmark Game (CLBG)](https://benchmarksgame-team.pages.debian.net/benchmarksgame/index.html). To evaluate the impact of programming language, four different programming languages (C, C++, Rust, Go) are selected . Similarly, two browsers (Google Chrome and Mozilla Firefox) are chosen to assess the impact of browser on runtime performance and energy consumption of Android web applications.

[Android Runner](https://github.com/S2-group/android-runner/tree/master/AndroidRunner) is used to orchestrate the experiment. [R Studio](https://posit.co/) is used for data analysis. 

## Structure of The Project
The basic structure of this project followed Android Runner repository project structure with several addition: 

- subject: contains every benchmark functions that are converted to Wasm and embedded to HTML
- source: contains original source code of benchmark functions
- data: contains data generated from the experiment
- config: contains Android Runner config that are used in the experiment
- scripts: contains R scripts that are used in data analysis part

## Dependencies
- [Android Runner] (https://github.com/S2-group/android-runner/tree/master/AndroidRunner)
- [R Studio] (https://posit.co/download/rstudio-desktop/)
- [Python3] (Web Server)(https://www.python.org/downloads/)
- Compiler: [Wasm-Pack](https://github.com/rustwasm/wasm-pack) (for Rust), [Emscripten](https://emscripten.org/docs/getting_started/downloads.html) (for C, C++), [Go Compiler](https://pkg.go.dev/cmd/compile) (Go)

## Getting Started

### Subject Compilation to WASM

The ready-for-experiment subject already provided in `subject` folder. However, if you would like to compile the source code yourself you can use the original source code in `source` folder. 

You can follow these tutorial to compile the programming languages into Wasm:
- [Rust to Wasm](https://developer.mozilla.org/en-US/docs/WebAssembly/Rust_to_wasm)
- [C, C++ to Wasm](https://developer.mozilla.org/en-US/docs/WebAssembly/C_to_wasm)
- [Go to Wasm](https://golangbot.com/webassembly-using-go/)

### Running the Experiment

In order to run the experiment, Android Runner need to be installed. You can follow [this tutorial](https://github.com/S2-group/android-runner/blob/master/CONTRIBUTING.md) to install and setup Android Runner. 

After Android Runner is installed, these functions in `Experiment.py` is modified to calculate start and end time of the experiment.
```
def start_profiling(self, device, path, run, *args, **kwargs):
        #FIXME: handle *args
        timeStamp = time.time()
        with open(paths.paths_dict().get('OUTPUT_DIR')+"Output.txt", "a") as text_file:
            text_file.write("start timestamp: {0}".format(timeStamp) + "\t") 
        self.logger.info('Earth start Timestamp ' + str(timeStamp))

        self.profilers.start_profiling(device, **kwargs)

def stop_profiling(self, device, path, run, *args, **kwargs):
        # FIXME: handle *args
        timeStamp = time.time()
        with open(paths.paths_dict().get('OUTPUT_DIR')+"Output.txt", "a") as text_file:
            text_file.write("end timestamp: {0}".format(timeStamp) + "\n") 
        self.logger.info('Earth end Timestamp ' + str(timeStamp))

        self.profilers.stop_profiling(device, **kwargs)
```

Before running the experiment, make sure you are connected to Android device from the orchestration machine which Android Runner is installed via
```
adb connect device_ip
``` 

After that, serve the subjects with a web server. You can go to `subject` folder and run 
```
python3 -m http.server
```

After device is connected, you need to modify experiment configuration. This configuration specificy treatment, subject and profiler for experiment. You can see the [instruction here](https://github.com/S2-group/android-runner). The configuration in our experiment can be seen in `config` folder. Don't forget to use the machine IP address to refer to the subject. 

To run the experiment, run command
```
python3 android-runner path_to_your_config.json
```

### Analyzing the Result

The result is analyzed using R Studio. The raw data that is generated in our experiment can be see in `data` folder. 

The analysis script can be seen in `scripts`. 

## Contributor
- Mohammad Rijal
- Yishak Abreham Gebremichael
- Shrawan Bishowkarma
- Esther Oluwatomi Adenekan
- Tanvir Tanmoy
