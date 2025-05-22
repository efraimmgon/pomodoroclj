# PomodoroCLJ

A Pomodoro timer program built with Clojure, designed to help you boost productivity and maintain focus using the Pomodoro Technique - intended to be run on the REPL.

## Features

- **Core Timer Functionality**
  - 25-minute Pomodoro sessions
  - 5-minute short breaks
  - 15-minute long breaks

- **User Interface**
  - Clean and intuitive design
  - Session type indicators
  - Visual progress tracking

- **Customization**
  - [TODO] Adjustable session durations
  - Sound and notification controls
  - [TODO] Customizable break intervals
  - [TODO] Session preferences

- **Statistics**
  - Daily Pomodoro tracking
  - [TODO] Weekly summaries
  - [TODO] Focus time analytics
  - [TODO] Productivity insights

## Getting Started

### Prerequisites

- Clojure >= 1.12.0

### Installation

1. Install clj cli:

```bash
brew install clojure/tools/clojure
```

2. Clone the repository:

```bash
git clone https://github.com/yourusername/pomodoroclj.git
```

### Usage

1. Start the REPL using your preferred Clojure editor:

```bash
clj -M:repl
```

2. Use the following commands to start, stop, and reset the timer:

```clj
(help)
(start "my first task!")
(stop)
(reset)
(skip)
```

## License

This project is licensed under the MIT License.