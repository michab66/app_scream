/*
 * Scream @ https://github.com/michab/dev_smack
 *
 * Copyright © 1998-2022 Michael G. Binz
 */

package de.michab.scream;

import urschleim.Continuation.Cont;
import urschleim.Continuation.Thunk;

/**
 * Followed the scheme spec in naming this class.  An alternate name would be
 * 'macro', also common is 'special form'.
 *
 * The implementation of this syntax facility is currently very simple.  All
 * syntactic elements of Scheme are mapped to straightforward method calls.
 * The problem here is that for each use of the 'do' machinery all syntax
 * analysis has to be done over and over again.  Solution is to switch to some
 * sort of primitive notation.  Basically a 'do' is a primitive operation for
 * all kinds of iteration sequences.  Primitives get evaluated (compiled?) once
 * and will be installed in the expression.  Another problem that could be
 * solved with this approach is the switch of the frontend language.  E.g.
 * more C-like notation could be possible then, where all of the special
 * processing is encapsulated in the frontend.  One advantage of Scheme being
 * the first implemented frontend is that this language by design tries to
 * implement the universal superset of language mechanisms so most other
 * languages should map onto this -- at least control structure.<br>
 *
 * Well, to be honest, this is not really new but is standard compiler design
 * since the seventies.  In a nutshell what we need is a compiled intermediate
 * language.<br>
 *
 * TODO: At the moment this class is pretty bare bones.  Only makes sense as a
 * base class for native java syntax implementations.  Will have to look into
 * syntax related scheme sections to implement the functionality specified there.
 *
 * @author Michael Binz
 */
public class Syntax
    extends Operation
{
    /**
     * The name of the type as used by error reporting.
     *
     * @see FirstClassObject#typename()
     */
    public static final String TYPE_NAME = "syntax";

    /**
     *
     */
    private static final Symbol ELSE = Symbol.createObject( "else" );

    /**
     * Default constructor.  Used for Java-defined specializations.
     *
     * @param name The new syntax' symbolic name.
     */
    private Syntax( Symbol name )
    {
        super( name );
    }

    /**
     * Constructor for Java-defined specializations.  The passed string names
     * the symbol bound to this syntax.
     *
     * @param name The string name of the symbol bound to the new syntax.  Used
     *        for error reporting.
     */
    protected Syntax( String name )
    {
        this( Symbol.createObject( name ) );
    }

    /**
     *
     * @param e
     * @param args
     * @param body
     * @throws RuntimeX
     */
    public Syntax( Environment e,
            FirstClassObject args,
            Cons body  )
                    throws RuntimeX
    {
        super( args, body, e );
    }

    /**
     * TODO fix comment
     * Activate, receives an array of FirstClassObjects instead of a Cons.  May
     * be handier for native java syntax implementations.<br>
     * Note that the different <code>activate</code> calls implement a chain of
     * responsibility -- one of them has to be overridden and implement the
     * actual functionality.  The default behavior of this final part in the
     * chain is to throw an <code>InternalError</code>.
     *
     * @param parent The parent environment.
     * @param arguments The argument list.
     * @return The result of the activation.
     * @throws RuntimeX In case an error occurred.
     * @throws InternalError In case this method is not overridden.
     */
    @Override
    protected FirstClassObject activate( Environment parent,
            FirstClassObject[] arguments )
                    throws RuntimeX
    {
        // TODO Below is the only reference to body from a subclass of Operation.
        // This is really a HACK since i have no better idea currently how to
        // differentiate btw Java and Scheme implemented Syntaxes/Operations.
        // Think.  We should get rid of the _body reference and set body as private
        // to Operation.
        if ( _body == Cons.NIL )
            return evaluate( compile( parent, arguments ), parent );

        return super.activate( parent, arguments );
    }

    @Override
    public Thunk _activate( Environment e, Cons args, Cont<FirstClassObject> c )
            throws RuntimeX
    {
        if ( _body == Cons.NIL )
        {
            var toEval = compile( e, args );

            return toEval.evaluate( e, c );
        }

        return super._activate( e, args, c );
    }

    /**
     * @return A string representation for this syntax.
     * @see FirstClassObject#toString
     */
    @Override
    public String toString()
    {
        return "<Syntax " + getName() + ">";
    }

    /**
     * Interprets a tail sequence.  A tail sequence is represented by a list of
     * expressions whose last one will be evaluated as trailing context.
     *
     * @param seq The list of expressions.
     * @param env The environment used for expression evaluation.
     * @return The result of the last expression in the list.
     * @throws RuntimeX If an error occurs while evaluating the expressions.
     */
    protected static FirstClassObject interpretTailSequence(
            FirstClassObject[] seq,
            Environment env )
                    throws
                    RuntimeX
    {
        return interpretTailSequence( seq, 0, env );
    }

    /**
     * Interprets a tail sequence.  A tail sequence is represented by a list of
     * expressions whose last one will be evaluated as trailing context.
     *
     * @param seq The list of expressions.
     * @param startIdx The index of the first expression to evaluate.
     * @param env The environment used for expression evaluation.
     * @return The result of the last expression in the list.
     * @throws RuntimeX If an error occurs while evaluating the expressions.
     */
    protected static FirstClassObject interpretTailSequence(
            FirstClassObject[] seq,
            int startIdx,
            Environment env )
                    throws
                    RuntimeX
    {
        for ( int i = startIdx ; i < seq.length -1 ; i++ )
            evaluate( seq[i], env );

        return evaluateTrailingContext( seq[ seq.length-1 ], env );
    }
}
