import {
    combineLatest,
    debounceTime,
    fromEvent,
    map,
    merge,
    mergeWith,
    pairwise,
    scan,
    share,
    startWith,
    Subject,
    switchMap,
    type Observable,
} from "rxjs";
import { fromFetch } from "rxjs/fetch";
import type { State, Output } from "./types";

const grammarInput = document.getElementById(
    "grammar-input",
) as HTMLTextAreaElement;
const stringInput = document.getElementById(
    "string-input",
) as HTMLTextAreaElement;
const dropDown = document.getElementById("parserSelect") as HTMLSelectElement;
const button = document.getElementById("runParser")!;
const saveButton = document.getElementById("saveParser")!;

type Action<T> = (_: T) => T;

const resetState: Action<State> = s => ({
    ...s,
    run: false,
});

// Create an Observable for keyboard input events
const input$: Observable<Action<State>> = fromEvent<KeyboardEvent>(
    grammarInput,
    "input",
).pipe(
    debounceTime(1000),
    map(event => (event.target as HTMLInputElement).value),
    map(value => s => resetState({ ...s, grammar: value })),
);

const stringToParse$: Observable<Action<State>> = fromEvent<KeyboardEvent>(
    stringInput,
    "input",
).pipe(
    debounceTime(1000),
    map(event => (event.target as HTMLInputElement).value),
    map(value => s => resetState({ ...s, string: value })),
);

const selectedParser$ = new Subject<string>();
const dropDownStream$: Observable<Action<State>> = merge(
    fromEvent(dropDown, "change").pipe(
        map(event => (event.target as HTMLSelectElement).value),
        mergeWith(selectedParser$),
        map(
            (value): Action<State> =>
                s =>
                    resetState({ ...s, selectedParser: value }),
        ),
    ),
);

const buttonStream$: Observable<Action<State>> = fromEvent(
    button,
    "click",
).pipe(map(() => s => ({ ...resetState(s), run: true })));

const saveButtonStream$: Observable<Action<State>> = fromEvent(
    saveButton,
    "click",
).pipe(map(() => s => ({ ...s, save: true })));

// State and Output are split up and this only returns an Output to avoid race
// conditions with this function overriding new values of the inputs (e.g.
// grammar, string) in the state
function getOutput(s: State): Observable<Action<Output>> {
    const body = new URLSearchParams();
    body.set("grammar", s.grammar);
    body.set("string", s.run ? s.string : "");
    body.set("selectedParser", s.run ? s.selectedParser : "");
    body.set("save", s.save ? "true" : "false"); 

    return fromFetch<
        Readonly<
            | { parsers: string; result: string; warnings: string; saveStatus?: string; fileName?: string }
            | { error: string }
        >
    >("/api/generate", {
        method: "POST",
        headers: {
            "Content-Type": "application/x-www-form-urlencoded",
        },
        body: body.toString(),
        selector: res => res.json(),
    }).pipe(
        map(res => {
            if ("error" in res)
                return o => ({ ...o, grammarParseError: res.error });
            const parsers = res.parsers.split(",");
            let saveStatusMsg = "";
            if (res.saveStatus === "success") {
                saveStatusMsg = `File saved: ${res.fileName}`;
            } else if (res.saveStatus === "error") {
                saveStatusMsg = `Save failed: ${res.fileName || "unknown error"}`;
            }
            return o => ({
                ...o,
                grammarParseError: "",
                warnings: res.warnings.split("\n"),
                parserOutput: res.result,
                parsers,
                selectedParser: parsers.includes(s.selectedParser)
                    ? s.selectedParser
                    : (parsers[0] ?? ""),
                    saveStatus: saveStatusMsg
            });
        }),
    );
}

interface ExtendedOutput extends Output {
    saveStatus: string;
}

const initialState: State = {
    grammar: "",
    string: "",
    selectedParser: "",
    run: false,
    save: false,
};

const initialOutput: Output = {
    grammarParseError: "",
    parsers: [],
    parserOutput: "",
    warnings: [],
    saveStatus: "",
};

function main() {
    const selectElement = document.getElementById("parserSelect")!;
    const grammarParseErrorOutput = document.getElementById(
        "grammar-parse-error-output",
    ) as HTMLOutputElement;
    const parserOutput = document.getElementById(
        "parser-output",
    ) as HTMLOutputElement;
    const validateOutput = document.getElementById(
        "validate-output",
    ) as HTMLOutputElement;
    const saveStatusOutput = document.getElementById(
        "save-status-output",
    ) as HTMLOutputElement;

    // Subscribe to the input Observable to listen for changes
    const state$ = merge(
        input$,
        dropDownStream$,
        stringToParse$,
        buttonStream$,
        saveButtonStream$
    ).pipe(
        scan((state, action) => action(state), initialState),
        share(),
    );
    const output$ = state$.pipe(
        switchMap(getOutput),
        scan((output, action) => action(output), initialOutput),
    );
    combineLatest([state$, output$])
        .pipe(
            map(([state, output]) => ({ ...state, ...output })),
            startWith({ ...initialState, ...initialOutput }),
            pairwise(),
            map(([s1, s2]) => ({
                ...s2,
                resetParsers:
                    s2.parsers.length !== s1.parsers.length ||
                    s2.parsers.some((x, i) => x !== s1.parsers[i]),
            })),
        )
        .subscribe(state => {
            if (state.resetParsers) {
                selectElement.replaceChildren(
                    ...state.parsers.map(optionText => {
                        const option = document.createElement("option");
                        option.value = optionText;
                        option.text = optionText;
                        return option;
                    }),
                );
                // if the <option> HTML elements are changed, the value of the
                // <select> element will reset to ""
                dropDown.value = state.selectedParser;
                selectedParser$.next(state.selectedParser);
            }

            grammarParseErrorOutput.value = state.grammarParseError;
            parserOutput.value = state.parserOutput;
            validateOutput.value = state.warnings.join("\n");
            saveStatusOutput.value = state.saveStatus;
        });
}
if (typeof window !== "undefined") {
    window.addEventListener("load", main);
}
