/*
 * Scream @ https://github.com/michab/dev_smack
 *
 * Copyright © 1998-2022 Michael G. Binz
 */
package de.michab.scream;

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
    final public Lambda compile( Environment parent, Cons args )
            throws RuntimeX
    {
        return _compile( parent, args );
    }
    @Override
    public FirstClassObject activate( Environment parent,
            Cons arguments )
                    throws RuntimeX
    {
        var λ = _compile( parent, arguments );

        return FirstClassObject.evaluate( λ, parent );
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
}
