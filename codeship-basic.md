## Codeship Basic

**Note: Codeship Basic is not supported at the moment**

Under *Tests*, add these setup and test commands:

Setup Commands:

    source /dev/stdin <<< "$(curl -sSL https://raw.githubusercontent.com/codeship/scripts/master/languages/erlang.sh)"


Test Commands:

    ./run.sh


Under *Environment*, create a new Environment Variable:

    PATH=$HOME/.beamup/cli:$PATH
    ERLANG_VERSION=20.1.2
