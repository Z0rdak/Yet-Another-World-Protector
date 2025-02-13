# Contributing to YAWP

Thank you for your interest in contributing to YAWP! 

Contributions help improve the mod and ensure compatibility with other mods. Please read the guidelines below before submitting a pull request or issue.

## 📝 How to Contribute

### 🐛 Reporting Bugs
If you've found a bug, please open an issue with the following details:
- **Description**: What happened? What did you expect to happen?
- **Steps to Reproduce**: Provide a step-by-step guide to reproduce the issue.
- **Logs & Crash Reports**: Include relevant logs or crash reports (use a pastebin-like service if needed).
- **Mod Version & Environment**: YAWP version, Minecraft version, Forge/Fabric version, and other relevant mods installed.

### 💡 Suggesting Features
Feature requests are welcome! When suggesting a new feature:
- Explain the **use case** and why it would benefit the mod.
- If possible, suggest an **implementation approach** or reference existing mods with similar features.
- Keep discussions constructive and open-ended for potential improvements.

Alternatively, visit our [Discord](https://discord.gg/d7hArKCUtm) server and report your issue or suggest a new feature there.

## ⚙️ Development Setup

1. Clone the repository:
   ```sh
   git clone https://github.com/Z0rdak/Yet-Another-World-Protector.git
   ```
2. Set up the development environment:
    - Ensure you have JDK 17 (for 1.20.1) or JDK 21 (for 1.21.1+) installed.
    - Import the project into your IDE (IntelliJ IDEA recommended).
    - Let gradle to its job importing the project and start coding :-)
3. Build the project:
   ```sh
   ./gradlew build
   ```

## 🖋️ Code Style & Conventions
- Follow standard **Java conventions** (use proper naming, avoid unnecessary complexity).
- Use spaces for indentation, **not tabs**.
- Keep methods **short and focused**—avoid large, complex methods.
- Keep compatibility with **Forge, Fabric and NeoForge** where applicable.

## 🔄 Pull Request Guidelines
1. **Create a new branch** for your feature or bug fix:
   ```sh
   git checkout -b feat/your-feature
   ```
2. **Please target the [multi/1.21.4 branch](https://github.com/Z0rdak/Yet-Another-World-Protector/tree/multi/1.21.4)**. Backports to supported versions are usually done by me, but feel free todo so =). 
4. **Keep PRs focused**—avoid bundling multiple changes in a single PR.
3. **Write clear commit messages** describing what each change does.
4. **Test your changes** before submitting.
5. If your PR references an issue, **link it in the description** (e.g., `Closes #42`).

## 📜 License
By contributing, you agree that your contributions will be licensed under the same license as YAWP.

Thank you for helping improve YAWP! 🚀
