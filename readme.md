# BeamUp Benchmarks


[![Travis](https://img.shields.io/travis/beamup-io/benchmark.svg?label=Travis%20CI)](http://travis-ci.org/beamup-io/benchmark)

[![CircleCI](https://img.shields.io/circleci/project/github/beamup-io/benchmark.svg?label=CircleCI)](https://circleci.com/gh/beamup-io/benchmark)

[![Codeship](https://img.shields.io/codeship/75d89310-b84e-0135-b223-3a46fb05df8d.svg?label=Codeship)](https://app.codeship.com/projects/258993/)

[![Wercker](https://img.shields.io/badge/Wercker-likely%20passing-brightgreen.svg)](https://app.wercker.com/albertzak/benchmark/runs)

[![Semaphore](https://img.shields.io/badge/Semaphore-likely%20passing-brightgreen.svg)](https://semaphoreci.com/albertzak/benchmark)

[![Shippable](https://img.shields.io/badge/Shippable-likely%20passing-brightgreen.svg)](https://app.shippable.com/subs/github/beamup-io/dashboard)


1. First, start the server where it's accessible from anywhere: `node ./server.js`

2. Update `$BENCHMARK_URL` in `./run.sh`: `BENCHMARK_URL=http://163.172.147.195:8080`

3. For each CI Provider, set `BENCHMARK_SUBJECT=<CiProviderName>`

4. Increment `benchmark_revision` in `./run.sh`

5. Watch results pour in. They are also written to `./results.json`
