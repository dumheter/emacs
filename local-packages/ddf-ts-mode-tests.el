;;; ddf-ts-mode-tests.el --- DDF mode tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Run with emacs --batch -Q -L local-packages
;; -l local-packages/ddf-ts-mode-tests.el -f ert-run-tests-batch-and-exit.
;; The bundled grammar is built on first use if it is not installed.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'ddf-ts-mode)

(defmacro ddf-ts-mode-tests--with-buffer (source &rest body)
  "Parse SOURCE in a temporary DDF buffer, then evaluate BODY."
  (declare (indent 1) (debug t))
  `(with-temp-buffer
     (insert ,source)
     (ddf-ts-mode)
     (should-not (treesit-node-check (treesit-buffer-root-node) 'has-error))
     ,@body))

(defun ddf-ts-mode-tests--face (text face)
  "Assert that TEXT starts with FACE in the current buffer."
  (goto-char (point-min))
  (search-forward text)
  (let ((actual (get-text-property (- (point) (length text)) 'face)))
    (should (if (listp actual) (memq face actual) (eq face actual)))))

(ert-deftest ddf-ts-mode-declarations-and-defaults ()
  (ddf-ts-mode-tests--with-buffer
      "import \"Example/Types.ddf\";
module Example;
namespace fb.example;
#include <Example/Module.h>
/* block comment */ // line comment
[Example.Attribute(\"positional\", Min=-1, Max=1.0e+2f)]
extendable enum Choice {
  First = -1,
  [Browsable(false)] Second = 1 << 2,
  Third = 0x10u | 0b10,
}
extend Choice { Fourth, }
extendable OldChoice { OldFirst, OldSecond, }
[Flag, OtherFlag(Value=true)]
abstract meta class ExampleType : Base, Interface {
  shared meta ExampleType Children[];
  override uint Count = 3;
  string Text = \"first\\\"part\" \"second
part\";
  float Scale = -.5f;
  Choice DefaultChoice = First;
  bool Enabled = true;
  Object Reference = null;
  int Numbers[] = [1, 2, 3,];
  Settings Value = { Count = 1, Nested = { Text = \"x\" }, Items = [false, true] };
  Settings Positional = { true, false, };
  Vec3 Position = Vec3(1.0f, 2.0f, 3.0f);
  inline SharedFields;
}
extend class ExampleType as AdditionalFields { int Extra; };
native struct Opaque;
interface Interface {};
global event Category.Changed { int Payload; }
event Category.Legacy(int payload)
message Category.Empty;
message Category.Pointer { class NativeThing* Value; Example::Thing* Values[]; }
hash Symbol;
hash NamedSymbol \"name\";
Settings Assets/Example = { Value = { Count = 1 }, Parents = [Assets/Parent] }
"))

(ert-deftest ddf-ts-mode-functions-and-entities ()
  (ddf-ts-mode-tests--with-buffer
      "delegate void Callback(bool value);
native function bool Read([Description(\"value\")] ref Item item, int count = 1);
function void Empty() {}
class Functions {
  function fragment void OnChanged();
  meta native functiondecl static void Update(ArrayEditor<Item> items);
  native functiondecl override bool Query() const;
  observer Payload Handler = { Read, Assets/Pass, Realm_Server };
  event Payload Changed = Handler;
}
extendable entity ExampleEntity {
  data { output bool Result; dynamic int Value; link EntityData Target; }
  InputEvent:
  client event Start;
  OutputEvent:
  server event Done;
}
extend entity ExampleEntity as Extra {
  data { int Value; }
}
component ExampleComponent {}
"))

(ert-deftest ddf-ts-mode-preprocessor-between-attributes-and-members ()
  (ddf-ts-mode-tests--with-buffer
      "#if VERSION >= MAKE_VERSION(1, 2)
[Description(\"first\")]
#elif OTHER
[Description(\"second\")]
#else
[Description(\"third\")]
#endif
class Conditional {
#ifdef FLAG
  [Flag]
#endif
  int Value;
}
enum ConditionalChoice {
#ifndef FLAG
  First,
#else
  Second,
#endif
}
"))

(ert-deftest ddf-ts-mode-verbatim-code-does-not-leak ()
  (ddf-ts-mode-tests--with-buffer
      "/% namespace native { class Native {}; } %/
/@ #define NATIVE_VALUE 42 @/
/# public partial class Managed { int value = 8; } #/
struct Container {
/$
  // Brackets and declarations here are not DDF.
  int nativeArray[2] = { 3, 4 };
  const char* text = \"[Attribute] class NotDdf {};\";
  #if FLAG
  void* pointer;
  #endif
$/
  int Reflected = 5;
}
"
    (let ((root (treesit-buffer-root-node)))
      (should (= 3 (length (treesit-query-capture root '((cpp_block) @block)))))
      (should (= 1 (length (treesit-query-capture root '((csharp_block) @block)))))
      (should (= 1 (length (treesit-query-capture root '((field_declaration) @field)))))
      (should (= 1 (length (treesit-query-capture root '((number_literal) @number))))))))

(ert-deftest ddf-ts-mode-verbatim-code-delimiters ()
  (dolist (source '("/$$/" "/%%/" "/@@/" "/##/"
                    "/$ #if FLAG $/" "/% // comment %/"
                    "/@#define FLAG 1@/" "/# // comment #/"
                    "/$ dollar $$/" "/% percent %%/"
                    "/@ at @@/" "/# hash ##/"))
    (ddf-ts-mode-tests--with-buffer source
      (font-lock-ensure)
      (ddf-ts-mode-tests--face source 'font-lock-doc-face))))

(ert-deftest ddf-ts-mode-highlighting ()
  (let ((treesit-font-lock-level 4))
    (ddf-ts-mode-tests--with-buffer
        "// comment
[Range(Min=0.25f, Mode=SomeMode)]
abstract class Highlighted : Base {
  [Description(\"a string\")]
  shared Item Field = DefaultItem;
  bool Enabled = true;
  int Shift = 1 << 2;
  function void Method(int parameter);
}
extend Highlighted { int Extra; }
extendable LegacyChoice { LegacyFirst, Alias = LegacyFirst, Bits = Alias | LegacyFirst, }
"
      (font-lock-ensure)
      (dolist (case '(("// comment" font-lock-comment-face)
                      ("Range" font-lock-preprocessor-face)
                      ("Min" font-lock-property-use-face)
                      ("SomeMode" font-lock-constant-face)
                      ("0.25f" font-lock-number-face)
                      ("abstract" font-lock-keyword-face)
                      ("Highlighted" font-lock-type-face)
                      ("Base" font-lock-type-face)
                      ("\"a string\"" font-lock-string-face)
                      ("shared" font-lock-keyword-face)
                      ("Item Field" font-lock-type-face)
                      ("Field" font-lock-variable-name-face)
                      ("DefaultItem" font-lock-constant-face)
                      ("true" font-lock-constant-face)
                      ("<<" font-lock-operator-face)
                      ("Method" font-lock-function-name-face)
                      ("parameter" font-lock-variable-name-face)
                      ("extendable" font-lock-keyword-face)
                      ("LegacyChoice" font-lock-type-face)
                      ("LegacyFirst, Bits" font-lock-constant-face)
                      ("Alias |" font-lock-constant-face)))
        (apply #'ddf-ts-mode-tests--face case))
      (goto-char (point-min))
      (search-forward "extend Highlighted")
      (should (eq (get-text-property (- (point) (length "Highlighted")) 'face)
                  'font-lock-type-face)))))

(ert-deftest ddf-ts-mode-incremental-highlighting ()
  (ddf-ts-mode-tests--with-buffer "class Edited { bool Enabled = true; }"
    (font-lock-ensure)
    (ddf-ts-mode-tests--face "true" 'font-lock-constant-face)
    (delete-region (- (point) 4) (point))
    (insert "\"changed\"")
    (font-lock-ensure)
    (ddf-ts-mode-tests--face "\"changed\"" 'font-lock-string-face)
    (should-not (treesit-node-check (treesit-buffer-root-node) 'has-error))))

(ert-deftest ddf-ts-mode-file-association ()
  (with-temp-buffer
    (setq buffer-file-name "example.ddf")
    (set-auto-mode)
    (should (eq major-mode 'ddf-ts-mode))))

(ert-deftest ddf-ts-mode-indentation-and-imenu ()
  (ddf-ts-mode-tests--with-buffer "class Indented {\nint Value;\n}\n"
    (indent-region (point-min) (point-max))
    (should (equal (buffer-string) "class Indented {\n    int Value;\n}\n"))
    (should (assoc "Types" (funcall imenu-create-index-function)))))

(ert-deftest ddf-ts-mode-missing-compiler-is-explicit ()
  (cl-letf (((symbol-function 'executable-find) (lambda (_) nil)))
    (should-error (ddf-ts-mode-install-grammar) :type 'user-error)))

(provide 'ddf-ts-mode-tests)
;;; ddf-ts-mode-tests.el ends here
