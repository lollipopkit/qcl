// Dart 版本：斐波那契测试与基准
// 用法：
//   - 仅测试：dart run examples/fibonacci_test.dart --test-only
//   - 基准（默认 iters=50000, n=30）：
//       dart run examples/fibonacci_test.dart
//     或：
//       dart run examples/fibonacci_test.dart --iters=80000 --n=30
//   - 生成原生可执行后基准（推荐更稳定的计时）：
//       dart compile exe -O2 -o /tmp/fib_bench examples/fibonacci_test.dart
//       /usr/bin/time -f "Dart: real %e user %U sys %S" /tmp/fib_bench --iters=50000 --n=30

import 'dart:io';

int fibRecursive(int n) {
  if (n <= 1) return n;
  return fibRecursive(n - 1) + fibRecursive(n - 2);
}

int fibIterative(int n) {
  if (n <= 1) return n;
  var a = 0;
  var b = 1;
  for (var i = 2; i <= n; i++) {
    final t = a + b;
    a = b;
    b = t;
  }
  return b;
}

void runTests() {
  final expected = <int>[0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55];
  for (var i = 0; i < expected.length; i++) {
    final it = fibIterative(i);
    if (it != expected[i]) {
      stderr.writeln('iterative failed at n=$i, got $it, want ${expected[i]}');
      exit(1);
    }
    final rec = fibRecursive(i);
    if (rec != expected[i]) {
      stderr.writeln('recursive failed at n=$i, got $rec, want ${expected[i]}');
      exit(1);
    }
  }
  print('All Fibonacci tests passed');
}

Map<String, String> _parseArgs(List<String> args) {
  final map = <String, String>{};
  for (final a in args) {
    final s = a.startsWith('--') ? a.substring(2) : a;
    final eq = s.indexOf('=');
    if (eq == -1) {
      map[s] = 'true';
    } else {
      map[s.substring(0, eq)] = s.substring(eq + 1);
    }
  }
  return map;
}

void main(List<String> args) {
  final opts = _parseArgs(args);
  final testOnly = opts['test-only'] == 'true';
  final iters = int.tryParse(opts['iters'] ?? '') ?? 50000;
  final n = int.tryParse(opts['n'] ?? '') ?? 30;

  runTests();
  if (testOnly) return;

  // 仅基准迭代实现，与 QCL 的 fibonacci_bench.qcl 对齐
  final sw = Stopwatch()..start();
  var acc = 0;
  for (var i = 0; i < iters; i++) {
    acc = fibIterative(n);
  }
  sw.stop();

  // 输出摘要与耗时（微秒/毫秒/秒）
  final us = sw.elapsedMicroseconds;
  final ms = (us / 1000).toStringAsFixed(3);
  final sec = (us / 1e6).toStringAsFixed(6);
  print('Summary: {"iters": $iters, "n": $n, "fib_n": $acc}');
  print('Elapsed: ${us}us (${ms}ms, ${sec}s)');
}

