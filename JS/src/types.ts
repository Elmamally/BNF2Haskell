export type State = Readonly<{
    grammar: string;
    string: string;
    selectedParser: string;
    run: boolean;
    save: boolean 
}>;
export type Output = Readonly<{
    grammarParseError: string;
    parsers: readonly string[];
    parserOutput: string;
    warnings: readonly string[];
    saveStatus: string;
}>;
