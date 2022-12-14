/*
 * zrc - the Zirco compiler
 * Copyright (C) 2022  LogN
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 */

import lex from "../index";
import { TokenTypes } from "../lex";

describe("lexer export", () => {
    it("works as expected", () =>
        expect(lex("2 + 2 = 4")).toEqual([
            ["2", { type: TokenTypes.Number, position: { start: 0, end: 1 } }],
            ["+", { type: TokenTypes.Operator, position: { start: 2, end: 3 } }],
            ["2", { type: TokenTypes.Number, position: { start: 4, end: 5 } }],
            ["=", { type: TokenTypes.Operator, position: { start: 6, end: 7 } }],
            ["4", { type: TokenTypes.Number, position: { start: 8, end: 9 } }]
        ]));
});
