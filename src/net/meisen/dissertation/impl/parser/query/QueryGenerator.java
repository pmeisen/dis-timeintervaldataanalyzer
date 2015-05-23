package net.meisen.dissertation.impl.parser.query;

import gnu.trove.set.hash.TIntHashSet;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import net.meisen.dissertation.exceptions.QueryParsingException;
import net.meisen.dissertation.impl.parser.query.add.AddQuery;
import net.meisen.dissertation.impl.parser.query.add.AddType;
import net.meisen.dissertation.impl.parser.query.alive.AliveQuery;
import net.meisen.dissertation.impl.parser.query.assign.AssignQuery;
import net.meisen.dissertation.impl.parser.query.delete.DeleteQuery;
import net.meisen.dissertation.impl.parser.query.drop.DropQuery;
import net.meisen.dissertation.impl.parser.query.drop.DropType;
import net.meisen.dissertation.impl.parser.query.generated.QueryGrammarBaseListener;
import net.meisen.dissertation.impl.parser.query.generated.QueryGrammarParser;
import net.meisen.dissertation.impl.parser.query.generated.QueryGrammarParser.CompDescValueTupelContext;
import net.meisen.dissertation.impl.parser.query.generated.QueryGrammarParser.CompDescriptorEqualContext;
import net.meisen.dissertation.impl.parser.query.generated.QueryGrammarParser.CompDescriptorFormulaAtomContext;
import net.meisen.dissertation.impl.parser.query.generated.QueryGrammarParser.CompDescriptorFormulaContext;
import net.meisen.dissertation.impl.parser.query.generated.QueryGrammarParser.CompDimAggrFunctionContext;
import net.meisen.dissertation.impl.parser.query.generated.QueryGrammarParser.CompDimMathMeasureAtomContext;
import net.meisen.dissertation.impl.parser.query.generated.QueryGrammarParser.CompDimMathMeasureContext;
import net.meisen.dissertation.impl.parser.query.generated.QueryGrammarParser.CompDimMeasureAtomContext;
import net.meisen.dissertation.impl.parser.query.generated.QueryGrammarParser.CompDimMeasureContext;
import net.meisen.dissertation.impl.parser.query.generated.QueryGrammarParser.CompGroupExcludeContext;
import net.meisen.dissertation.impl.parser.query.generated.QueryGrammarParser.CompGroupIncludeContext;
import net.meisen.dissertation.impl.parser.query.generated.QueryGrammarParser.CompIdContext;
import net.meisen.dissertation.impl.parser.query.generated.QueryGrammarParser.CompLowAggrFunctionContext;
import net.meisen.dissertation.impl.parser.query.generated.QueryGrammarParser.CompLowMeasureAtomContext;
import net.meisen.dissertation.impl.parser.query.generated.QueryGrammarParser.CompLowMeasureContext;
import net.meisen.dissertation.impl.parser.query.generated.QueryGrammarParser.CompMathAggrFunctionContext;
import net.meisen.dissertation.impl.parser.query.generated.QueryGrammarParser.CompMathMeasureAtomContext;
import net.meisen.dissertation.impl.parser.query.generated.QueryGrammarParser.CompMathMeasureContext;
import net.meisen.dissertation.impl.parser.query.generated.QueryGrammarParser.CompMemberEqualContext;
import net.meisen.dissertation.impl.parser.query.generated.QueryGrammarParser.CompNamedDimMathMeasureContext;
import net.meisen.dissertation.impl.parser.query.generated.QueryGrammarParser.CompNamedLowMeasureContext;
import net.meisen.dissertation.impl.parser.query.generated.QueryGrammarParser.CompStructureElementContext;
import net.meisen.dissertation.impl.parser.query.generated.QueryGrammarParser.CompValueElementContext;
import net.meisen.dissertation.impl.parser.query.generated.QueryGrammarParser.ExprAddContext;
import net.meisen.dissertation.impl.parser.query.generated.QueryGrammarParser.ExprAggregateContext;
import net.meisen.dissertation.impl.parser.query.generated.QueryGrammarParser.ExprAliveContext;
import net.meisen.dissertation.impl.parser.query.generated.QueryGrammarParser.ExprAssignContext;
import net.meisen.dissertation.impl.parser.query.generated.QueryGrammarParser.ExprAssignMultipleRolesContext;
import net.meisen.dissertation.impl.parser.query.generated.QueryGrammarParser.ExprAssignSingleRoleContext;
import net.meisen.dissertation.impl.parser.query.generated.QueryGrammarParser.ExprCompContext;
import net.meisen.dissertation.impl.parser.query.generated.QueryGrammarParser.ExprDeleteContext;
import net.meisen.dissertation.impl.parser.query.generated.QueryGrammarParser.ExprDropContext;
import net.meisen.dissertation.impl.parser.query.generated.QueryGrammarParser.ExprGetContext;
import net.meisen.dissertation.impl.parser.query.generated.QueryGrammarParser.ExprGrantContext;
import net.meisen.dissertation.impl.parser.query.generated.QueryGrammarParser.ExprGroupContext;
import net.meisen.dissertation.impl.parser.query.generated.QueryGrammarParser.ExprInsertContext;
import net.meisen.dissertation.impl.parser.query.generated.QueryGrammarParser.ExprIntervalContext;
import net.meisen.dissertation.impl.parser.query.generated.QueryGrammarParser.ExprLoadContext;
import net.meisen.dissertation.impl.parser.query.generated.QueryGrammarParser.ExprLoadSetPropertyContext;
import net.meisen.dissertation.impl.parser.query.generated.QueryGrammarParser.ExprMeasureContext;
import net.meisen.dissertation.impl.parser.query.generated.QueryGrammarParser.ExprModifyContext;
import net.meisen.dissertation.impl.parser.query.generated.QueryGrammarParser.ExprRemoveContext;
import net.meisen.dissertation.impl.parser.query.generated.QueryGrammarParser.ExprRemoveMultipleRolesContext;
import net.meisen.dissertation.impl.parser.query.generated.QueryGrammarParser.ExprRemoveSingleRoleContext;
import net.meisen.dissertation.impl.parser.query.generated.QueryGrammarParser.ExprRevokeContext;
import net.meisen.dissertation.impl.parser.query.generated.QueryGrammarParser.ExprSelectContext;
import net.meisen.dissertation.impl.parser.query.generated.QueryGrammarParser.ExprSelectRecordsContext;
import net.meisen.dissertation.impl.parser.query.generated.QueryGrammarParser.ExprSelectTimeSeriesContext;
import net.meisen.dissertation.impl.parser.query.generated.QueryGrammarParser.ExprSetBulkLoadContext;
import net.meisen.dissertation.impl.parser.query.generated.QueryGrammarParser.ExprSetPasswordContext;
import net.meisen.dissertation.impl.parser.query.generated.QueryGrammarParser.ExprStructureContext;
import net.meisen.dissertation.impl.parser.query.generated.QueryGrammarParser.ExprUnloadContext;
import net.meisen.dissertation.impl.parser.query.generated.QueryGrammarParser.ExprUnloadSetPropertyContext;
import net.meisen.dissertation.impl.parser.query.generated.QueryGrammarParser.ExprValuesContext;
import net.meisen.dissertation.impl.parser.query.generated.QueryGrammarParser.ExprWithPasswordContext;
import net.meisen.dissertation.impl.parser.query.generated.QueryGrammarParser.ExprWithPermissionsContext;
import net.meisen.dissertation.impl.parser.query.generated.QueryGrammarParser.ExprWithRolesContext;
import net.meisen.dissertation.impl.parser.query.generated.QueryGrammarParser.SelectorAliasContext;
import net.meisen.dissertation.impl.parser.query.generated.QueryGrammarParser.SelectorBooleanContext;
import net.meisen.dissertation.impl.parser.query.generated.QueryGrammarParser.SelectorDateIntervalContext;
import net.meisen.dissertation.impl.parser.query.generated.QueryGrammarParser.SelectorDateIntervalWithNullContext;
import net.meisen.dissertation.impl.parser.query.generated.QueryGrammarParser.SelectorDateValueContext;
import net.meisen.dissertation.impl.parser.query.generated.QueryGrammarParser.SelectorDescriptorIdContext;
import net.meisen.dissertation.impl.parser.query.generated.QueryGrammarParser.SelectorDimAggrFunctionNameContext;
import net.meisen.dissertation.impl.parser.query.generated.QueryGrammarParser.SelectorIntIdListContext;
import net.meisen.dissertation.impl.parser.query.generated.QueryGrammarParser.SelectorIntIntervalContext;
import net.meisen.dissertation.impl.parser.query.generated.QueryGrammarParser.SelectorIntIntervalWithNullContext;
import net.meisen.dissertation.impl.parser.query.generated.QueryGrammarParser.SelectorIntValueContext;
import net.meisen.dissertation.impl.parser.query.generated.QueryGrammarParser.SelectorIntervalDefContext;
import net.meisen.dissertation.impl.parser.query.generated.QueryGrammarParser.SelectorIntervalRelationContext;
import net.meisen.dissertation.impl.parser.query.generated.QueryGrammarParser.SelectorLimitContext;
import net.meisen.dissertation.impl.parser.query.generated.QueryGrammarParser.SelectorLowAggrFunctionNameContext;
import net.meisen.dissertation.impl.parser.query.generated.QueryGrammarParser.SelectorMathAggrFunctionNameContext;
import net.meisen.dissertation.impl.parser.query.generated.QueryGrammarParser.SelectorMemberContext;
import net.meisen.dissertation.impl.parser.query.generated.QueryGrammarParser.SelectorModelIdContext;
import net.meisen.dissertation.impl.parser.query.generated.QueryGrammarParser.SelectorNullValueContext;
import net.meisen.dissertation.impl.parser.query.generated.QueryGrammarParser.SelectorOffsetContext;
import net.meisen.dissertation.impl.parser.query.generated.QueryGrammarParser.SelectorValueContext;
import net.meisen.dissertation.impl.parser.query.generated.QueryGrammarParser.SelectorValueListContext;
import net.meisen.dissertation.impl.parser.query.get.GetQuery;
import net.meisen.dissertation.impl.parser.query.get.GetResultType;
import net.meisen.dissertation.impl.parser.query.grant.GrantQuery;
import net.meisen.dissertation.impl.parser.query.grant.GrantType;
import net.meisen.dissertation.impl.parser.query.insert.InsertQuery;
import net.meisen.dissertation.impl.parser.query.load.LoadQuery;
import net.meisen.dissertation.impl.parser.query.modify.ModifyQuery;
import net.meisen.dissertation.impl.parser.query.modify.ModifyType;
import net.meisen.dissertation.impl.parser.query.remove.RemoveQuery;
import net.meisen.dissertation.impl.parser.query.revoke.RevokeQuery;
import net.meisen.dissertation.impl.parser.query.revoke.RevokeType;
import net.meisen.dissertation.impl.parser.query.select.DescriptorComperator;
import net.meisen.dissertation.impl.parser.query.select.DimensionComperator;
import net.meisen.dissertation.impl.parser.query.select.IntervalRelation;
import net.meisen.dissertation.impl.parser.query.select.SelectQuery;
import net.meisen.dissertation.impl.parser.query.select.SelectResultType;
import net.meisen.dissertation.impl.parser.query.select.group.GroupExpression;
import net.meisen.dissertation.impl.parser.query.select.logical.LogicalOperator;
import net.meisen.dissertation.impl.parser.query.select.measures.ArithmeticOperator;
import net.meisen.dissertation.impl.parser.query.select.measures.DescriptorMathTree;
import net.meisen.dissertation.impl.parser.query.unload.UnloadQuery;
import net.meisen.dissertation.model.auth.permissions.DefinedPermission;
import net.meisen.dissertation.model.measures.AggregationFunctionHandler;
import net.meisen.dissertation.model.measures.IAggregationFunction;
import net.meisen.dissertation.model.measures.IDimAggregationFunction;
import net.meisen.dissertation.model.measures.ILowAggregationFunction;
import net.meisen.dissertation.model.measures.IMathAggregationFunction;
import net.meisen.dissertation.model.parser.query.IQuery;
import net.meisen.general.genmisc.exceptions.ForwardedRuntimeException;
import net.meisen.general.genmisc.types.Dates;
import net.meisen.general.genmisc.types.Objects;
import net.meisen.general.genmisc.types.Strings;

import org.antlr.v4.runtime.ParserRuleContext;
import org.antlr.v4.runtime.Token;
import org.antlr.v4.runtime.tree.ParseTree;
import org.antlr.v4.runtime.tree.TerminalNode;

/**
 * A generator to generate a {@code Query} from a {@code QueryGrammarParser}.
 * 
 * @see QueryGrammarParser
 * @see IQuery
 * 
 * @author pmeisen
 */
public class QueryGenerator extends QueryGrammarBaseListener {
	private final AggregationFunctionHandler aggFuncHandler;
	private final boolean optimize;

	private IQuery query;
	private boolean finalized = false;
	private DescriptorMathTree mathExpr = null;

	/**
	 * Generates a {@code QueryGenerator} which will trigger optimization after
	 * complete generation.
	 * 
	 * @param aggFuncHandler
	 *            the {@code AggregationFunctionHandler} used to resolve a used
	 *            aggregation, can be {@code null} if not functions should be
	 *            resolved
	 */
	public QueryGenerator(final AggregationFunctionHandler aggFuncHandler) {
		this(aggFuncHandler, true);
	}

	/**
	 * Generates a {@code QueryGenerator} which will optimize (i.e.
	 * {@code optimize} is {@code true}) or not optimize (i.e. {@code optimize}
	 * is {@code false}) the created query.
	 * 
	 * @param aggFuncHandler
	 *            the {@code AggregationFunctionHandler} used to resolve a used
	 *            aggregation, can be {@code null} if not functions should be
	 *            resolved
	 * @param optimize
	 *            {@code true} if the created query should be optimized,
	 *            otherwise {@code false}
	 */
	public QueryGenerator(final AggregationFunctionHandler aggFuncHandler,
			final boolean optimize) {
		this.aggFuncHandler = aggFuncHandler;
		this.optimize = optimize;
	}

	@Override
	public void enterExprAlive(final ExprAliveContext ctx) {
		if (this.query != null) {
			throw new ForwardedRuntimeException(QueryParsingException.class,
					1001);
		}

		this.query = new AliveQuery();
	}

	@Override
	public void exitExprAlive(final ExprAliveContext ctx) {
		finalized = true;
	}

	@Override
	public void enterExprLoad(final ExprLoadContext ctx) {
		if (this.query != null) {
			throw new ForwardedRuntimeException(QueryParsingException.class,
					1001);
		}

		this.query = new LoadQuery();
	}

	@Override
	public void enterExprDelete(final ExprDeleteContext ctx) {
		if (this.query != null) {
			throw new ForwardedRuntimeException(QueryParsingException.class,
					1001);
		}

		this.query = new DeleteQuery();
	}

	@Override
	public void exitExprDelete(final ExprDeleteContext ctx) {
		finalized = true;
	}

	@Override
	public void exitExprLoad(final ExprLoadContext ctx) {
		final LoadQuery q = q(LoadQuery.class);

		if (ctx.VALUE() != null) {
			q.setPath(getValue(ctx.VALUE()));
		}

		finalized = true;
	}

	@Override
	public void enterExprAdd(final ExprAddContext ctx) {
		if (this.query != null) {
			throw new ForwardedRuntimeException(QueryParsingException.class,
					1001);
		}

		this.query = new AddQuery();
	}

	@Override
	public void exitExprAdd(final ExprAddContext ctx) {
		final AddQuery q = q(AddQuery.class);

		// set the type
		if (ctx.TYPE_ROLE() != null) {
			q.setEntityType(AddType.ROLE);
		} else if (ctx.TYPE_USER() != null) {
			q.setEntityType(AddType.USER);
		}

		// set the name
		if (ctx.VALUE() != null) {
			q.setEntityName(getValue(ctx.VALUE()));
		}

		finalized = true;
	}

	@Override
	public void enterExprAssign(final ExprAssignContext ctx) {
		if (this.query != null) {
			throw new ForwardedRuntimeException(QueryParsingException.class,
					1001);
		}

		this.query = new AssignQuery();
	}

	@Override
	public void exitExprAssign(final ExprAssignContext ctx) {
		final AssignQuery q = q(AssignQuery.class);

		// set the name
		if (ctx.VALUE() != null) {
			q.setEntityName(getValue(ctx.VALUE()));
		}

		finalized = true;
	}

	@Override
	public void exitExprAssignSingleRole(final ExprAssignSingleRoleContext ctx) {
		final AssignQuery q = q(AssignQuery.class);

		if (ctx.VALUE() != null) {
			final String role = getValue(ctx.VALUE());
			q.setRoles(Arrays.asList(new String[] { role }));
		}
	}

	@Override
	public void exitExprAssignMultipleRoles(
			final ExprAssignMultipleRolesContext ctx) {
		final AssignQuery q = q(AssignQuery.class);

		final List<String> roles = getValues(ctx.selectorValueList());
		q.setRoles(roles);
	}

	@Override
	public void enterExprRemove(final ExprRemoveContext ctx) {
		if (this.query != null) {
			throw new ForwardedRuntimeException(QueryParsingException.class,
					1001);
		}

		this.query = new RemoveQuery();
	}

	@Override
	public void exitExprRemove(final ExprRemoveContext ctx) {
		final RemoveQuery q = q(RemoveQuery.class);

		// set the name
		if (ctx.VALUE() != null) {
			q.setEntityName(getValue(ctx.VALUE()));
		}

		finalized = true;
	}

	@Override
	public void exitExprRemoveSingleRole(final ExprRemoveSingleRoleContext ctx) {
		final RemoveQuery q = q(RemoveQuery.class);

		if (ctx.VALUE() != null) {
			final String role = getValue(ctx.VALUE());
			q.setRoles(Arrays.asList(new String[] { role }));
		}
	}

	@Override
	public void exitExprRemoveMultipleRoles(
			final ExprRemoveMultipleRolesContext ctx) {
		final RemoveQuery q = q(RemoveQuery.class);

		final List<String> roles = getValues(ctx.selectorValueList());
		q.setRoles(roles);
	}

	@Override
	public void enterExprGrant(final ExprGrantContext ctx) {
		if (this.query != null) {
			throw new ForwardedRuntimeException(QueryParsingException.class,
					1001);
		}

		this.query = new GrantQuery();
	}

	@Override
	public void exitExprGrant(final ExprGrantContext ctx) {
		final GrantQuery q = q(GrantQuery.class);

		// set the type
		if (ctx.TYPE_ROLE() != null) {
			q.setEntityType(GrantType.ROLE);
		} else if (ctx.TYPE_USER() != null) {
			q.setEntityType(GrantType.USER);
		}

		// set the name
		if (ctx.VALUE() != null) {
			q.setEntityName(getValue(ctx.VALUE()));
		}

		// set the permissions
		final List<DefinedPermission> perms = getPermissions(ctx
				.selectorValueList());
		q.setPermissions(perms);

		finalized = true;
	}

	@Override
	public void enterExprRevoke(final ExprRevokeContext ctx) {
		if (this.query != null) {
			throw new ForwardedRuntimeException(QueryParsingException.class,
					1001);
		}

		this.query = new RevokeQuery();
	}

	@Override
	public void exitExprRevoke(final ExprRevokeContext ctx) {
		final RevokeQuery q = q(RevokeQuery.class);

		// set the type
		if (ctx.TYPE_ROLE() != null) {
			q.setEntityType(RevokeType.ROLE);
		} else if (ctx.TYPE_USER() != null) {
			q.setEntityType(RevokeType.USER);
		}

		// set the name
		if (ctx.VALUE() != null) {
			q.setEntityName(getValue(ctx.VALUE()));
		}

		// set the permissions
		final List<DefinedPermission> perms = getPermissions(ctx
				.selectorValueList());
		q.setPermissions(perms);

		finalized = true;
	}

	@Override
	public void enterExprDrop(final ExprDropContext ctx) {
		if (this.query != null) {
			throw new ForwardedRuntimeException(QueryParsingException.class,
					1001);
		}

		this.query = new DropQuery();
	}

	@Override
	public void exitExprDrop(final ExprDropContext ctx) {
		final DropQuery q = q(DropQuery.class);

		// set the type
		if (ctx.TYPE_ROLE() != null) {
			q.setEntityType(DropType.ROLE);
		} else if (ctx.TYPE_USER() != null) {
			q.setEntityType(DropType.USER);
		} else if (ctx.TYPE_MODEL() != null) {
			q.setEntityType(DropType.MODEL);
		}

		// set the name
		if (ctx.VALUE() != null) {
			q.setEntityName(getValue(ctx.VALUE()));
		} else if (ctx.selectorModelId() != null) {
			q.setEntityName(getModelId(ctx.selectorModelId()));
		}

		finalized = true;
	}

	@Override
	public void enterExprModify(final ExprModifyContext ctx) {
		if (this.query != null) {
			throw new ForwardedRuntimeException(QueryParsingException.class,
					1001);
		}

		this.query = new ModifyQuery();
	}

	@Override
	public void exitExprModify(final ExprModifyContext ctx) {
		final ModifyQuery q = q(ModifyQuery.class);

		// set the type
		if (ctx.TYPE_MODEL() != null) {
			q.setType(ModifyType.MODEL);
		} else if (ctx.TYPE_USER() != null) {
			q.setType(ModifyType.USER);
		}

		// set the name
		if (ctx.VALUE() != null) {
			q.setEntityName(getValue(ctx.VALUE()));
		} else if (ctx.selectorModelId() != null) {
			q.setEntityName(getModelId(ctx.selectorModelId()));
		}

		finalized = true;
	}

	@Override
	public void exitExprSetPassword(final ExprSetPasswordContext ctx) {
		final ModifyQuery q = q(ModifyQuery.class);

		if (ctx.VALUE() != null) {
			q.setEntityValue(getValue(ctx.VALUE()));
		}
	}

	@Override
	public void exitExprSetBulkLoad(final ExprSetBulkLoadContext ctx) {
		final ModifyQuery q = q(ModifyQuery.class);

		if (ctx.selectorBoolean() != null) {
			q.setEntityValue(ctx.selectorBoolean().getText());
		}
	}

	@Override
	public void exitExprWithRoles(final ExprWithRolesContext ctx) {
		final AddQuery q = q(AddQuery.class);

		final List<String> roles = getValues(ctx.selectorValueList());
		q.setRoles(roles);
	}

	@Override
	public void exitExprWithPermissions(final ExprWithPermissionsContext ctx) {
		final AddQuery q = q(AddQuery.class);

		final List<DefinedPermission> perms = getPermissions(ctx
				.selectorValueList());
		q.setPermissions(perms);
	}

	@Override
	public void exitExprWithPassword(final ExprWithPasswordContext ctx) {
		final AddQuery q = q(AddQuery.class);

		if (ctx.VALUE() != null) {
			q.setEntityPassword(getValue(ctx.VALUE()));
		}
	}

	@Override
	public void enterExprUnload(final ExprUnloadContext ctx) {
		if (this.query != null) {
			throw new ForwardedRuntimeException(QueryParsingException.class,
					1001);
		}

		this.query = new UnloadQuery();
	}

	@Override
	public void exitExprUnload(final ExprUnloadContext ctx) {
		finalized = true;
	}

	@Override
	public void enterExprInsert(final ExprInsertContext ctx) {
		if (this.query != null) {
			throw new ForwardedRuntimeException(QueryParsingException.class,
					1001);
		}

		this.query = new InsertQuery();
	}

	@Override
	public void exitExprLoadSetProperty(final ExprLoadSetPropertyContext ctx) {
		final Map<String, Object> properties = new HashMap<String, Object>();
		resolveProperty(properties, ctx);

		final LoadQuery q = q(LoadQuery.class);
		q.setProperties(properties);
	}

	@Override
	public void exitExprUnloadSetProperty(final ExprUnloadSetPropertyContext ctx) {
		final Map<String, Object> properties = new HashMap<String, Object>();
		resolveProperty(properties, ctx);

		final UnloadQuery q = q(UnloadQuery.class);
		q.setProperties(properties);
	}

	@Override
	public void exitExprInsert(final ExprInsertContext ctx) {
		finalized = true;
	}

	@Override
	public void exitExprStructure(final ExprStructureContext ctx) {
		final InsertQuery q = q(InsertQuery.class);

		// determine the specified interval-types
		final List<CompStructureElementContext> structures = ctx
				.compStructureElement();

		final List<String> ids = new ArrayList<String>();
		final IntervalType[] types = new IntervalType[2];
		final int[] typePosition = new int[2];
		for (int i = 0; i < structures.size(); i++) {
			final CompStructureElementContext structure = structures.get(i);

			if (structure.selectorIntervalDef() != null) {
				final int pos = isStartIntervalType(structure
						.selectorIntervalDef()) ? 0 : 1;

				if (types[pos] == null) {
					types[pos] = resolveIntervalType(structure
							.selectorIntervalDef());
					typePosition[pos] = i;
				} else {
					throw new ForwardedRuntimeException(
							QueryParsingException.class, 1012, ctx.getText());
				}
			} else if (structure.selectorDescriptorId() != null) {
				ids.add(getDescriptorModelId(structure.selectorDescriptorId()));
			} else {
				throw new ForwardedRuntimeException(
						QueryParsingException.class, 1013, structure.getText());
			}
		}

		// set the read values
		q.setStart(typePosition[0], types[0]);
		q.setEnd(typePosition[1], types[1]);
		q.setDescriptorModels(ids);
	}

	@Override
	public void exitExprValues(final ExprValuesContext ctx) {
		final InsertQuery q = q(InsertQuery.class);

		final Object[] intervalValues = new Object[2];
		final List<CompValueElementContext> values = ctx.compValueElement();
		final int descSize = q.sizeOfDescriptorModelIds();
		final int expSize = descSize + 2;
		final List<String> data = new ArrayList<String>(descSize);
		for (int i = 0; i < values.size(); i++) {
			final CompValueElementContext value = values.get(i);
			final Object v = resolveValue(value);

			if (i >= expSize) {
				throw new ForwardedRuntimeException(
						QueryParsingException.class, 1014,
						Strings.trimSequence(value.getText(), "'"), i + 1);
			} else if (i == q.getStartPosition()) {
				if (value.selectorValue() != null) {
					throw new ForwardedRuntimeException(
							QueryParsingException.class, 1016, v,
							q.getStartPosition() + 1);
				}

				intervalValues[0] = v;
			} else if (i == q.getEndPosition()) {
				if (value.selectorValue() != null) {
					throw new ForwardedRuntimeException(
							QueryParsingException.class, 1016, v,
							q.getEndPosition() + 1);
				}

				intervalValues[1] = v;
			} else {
				data.add(v == null ? null : v.toString());
			}
		}

		// determine the value types
		final boolean startIsDate = intervalValues[0] == null
				|| intervalValues[0] instanceof Date;
		final boolean endIsDate = intervalValues[1] == null
				|| intervalValues[1] instanceof Date;
		final boolean startIsLong = intervalValues[0] == null
				|| intervalValues[0] instanceof Long;
		final boolean endIsLong = intervalValues[1] == null
				|| intervalValues[1] instanceof Long;

		// generate the interval
		final Interval<?> interval;
		if (startIsDate && endIsDate) {
			interval = new Interval<Date>(new DateIntervalValue(
					(Date) intervalValues[0]), q.getStartIntervalType(),
					new DateIntervalValue((Date) intervalValues[1]),
					q.getEndIntervalType());
		} else if (startIsLong && endIsLong) {
			interval = new Interval<Long>(new LongIntervalValue(
					(Long) intervalValues[0]), q.getStartIntervalType(),
					new LongIntervalValue((Long) intervalValues[1]),
					q.getEndIntervalType());
		} else {
			throw new ForwardedRuntimeException(QueryParsingException.class,
					1017, intervalValues[0], intervalValues[1]);
		}

		// validate the data
		if (data.size() != q.sizeOfDescriptorModelIds()) {
			throw new ForwardedRuntimeException(QueryParsingException.class,
					1018, data, q.getDescriptorModelIds());
		}

		// add the record
		q.addData(interval, data);
	}

	@Override
	public void enterExprGet(final ExprGetContext ctx) {
		if (this.query != null) {
			throw new ForwardedRuntimeException(QueryParsingException.class,
					1001);
		}

		this.query = new GetQuery();
	}

	@Override
	public void exitExprGet(final ExprGetContext ctx) {

		q(GetQuery.class).setResultType(resolveGetResultType(ctx));

		finalized = true;
	}

	@Override
	public void enterExprSelect(final ExprSelectContext ctx) {
		if (this.query != null) {
			throw new ForwardedRuntimeException(QueryParsingException.class,
					1001);
		}

		this.query = new SelectQuery();
	}

	@Override
	public void exitExprSelect(final ExprSelectContext ctx) {
		if (isOptimize()) {
			q(SelectQuery.class).optimize();
		}

		finalized = true;
	}

	/**
	 * Checks if the generation is finalized, i.e. {@code true} is returned.
	 * 
	 * @return the generation is finalized (i.e. {@code true}), if not finalized
	 *         {@code false} is returned
	 */
	public boolean isFinalized() {
		return this.finalized;
	}

	@Override
	public void enterCompNamedLowMeasure(final CompNamedLowMeasureContext ctx) {
		final String id = getAlias(ctx.selectorAlias());
		mathExpr = new DescriptorMathTree(id);
	}

	@Override
	public void exitCompNamedLowMeasure(final CompNamedLowMeasureContext ctx) {
		q(SelectQuery.class).addMeasure(mathExpr);
	}

	@Override
	public void exitCompId(final CompIdContext ctx) {
		final List<TerminalNode> idNodes = ctx.INT();

		final TIntHashSet ids = new TIntHashSet(idNodes.size());
		for (final TerminalNode node : idNodes) {
			ids.add(Integer.parseInt(node.getText()));
		}

		q(SelectQuery.class).setRecordIdFilter(ids.toArray());
	}

	@Override
	public void enterCompNamedDimMathMeasure(
			final CompNamedDimMathMeasureContext ctx) {
		final String id = getAlias(ctx.selectorAlias());
		mathExpr = new DescriptorMathTree(id);
	}

	@Override
	public void exitCompNamedDimMathMeasure(
			final CompNamedDimMathMeasureContext ctx) {
		q(SelectQuery.class).addMeasure(mathExpr);
	}

	@Override
	public void enterCompLowMeasure(final CompLowMeasureContext ctx) {

		// add the function to the tree
		if (ctx.selectorSecondMathOperator() != null) {
			final ArithmeticOperator ao = resolveArithmeticOperator(ctx
					.selectorSecondMathOperator());
			mathExpr.attach(ao);
		}
	}

	@Override
	public void exitCompLowMeasure(final CompLowMeasureContext ctx) {

		// check if we had an operator and we have to move up
		if (ctx.selectorSecondMathOperator() != null) {
			mathExpr.moveUp();
		}
	}

	@Override
	public void enterCompLowMeasureAtom(final CompLowMeasureAtomContext ctx) {
		if (ctx.selectorFirstMathOperator() != null) {
			final ArithmeticOperator ao = resolveArithmeticOperator(ctx
					.selectorFirstMathOperator());
			mathExpr.attach(ao);
		} else if (ctx.compLowAggrFunction() != null) {
			final CompLowAggrFunctionContext aggFuncCtx = ctx
					.compLowAggrFunction();
			mathExpr.attach(resolveAggregationFunction(
					aggFuncCtx.selectorLowAggrFunctionName(),
					ILowAggregationFunction.class));
		}
	}

	@Override
	public void exitCompLowMeasureAtom(final CompLowMeasureAtomContext ctx) {
		if (ctx.selectorFirstMathOperator() != null) {
			mathExpr.moveUp();
		} else if (ctx.compLowAggrFunction() != null) {
			mathExpr.moveUp();
		}
	}

	@Override
	public void enterCompDimMathMeasure(final CompDimMathMeasureContext ctx) {

		// add the function to the tree
		if (ctx.selectorSecondMathOperator() != null) {
			final ArithmeticOperator ao = resolveArithmeticOperator(ctx
					.selectorSecondMathOperator());
			mathExpr.attach(ao);
		}
	}

	@Override
	public void exitCompDimMathMeasure(final CompDimMathMeasureContext ctx) {

		// check if we had an operator and we have to move up
		if (ctx.selectorSecondMathOperator() != null) {
			mathExpr.moveUp();
		}
	}

	@Override
	public void enterCompDimMathMeasureAtom(
			final CompDimMathMeasureAtomContext ctx) {
		if (ctx.selectorFirstMathOperator() != null) {
			final ArithmeticOperator ao = resolveArithmeticOperator(ctx
					.selectorFirstMathOperator());
			mathExpr.attach(ao);
		}
	}

	@Override
	public void exitCompDimMathMeasureAtom(
			final CompDimMathMeasureAtomContext ctx) {
		if (ctx.selectorFirstMathOperator() != null) {
			mathExpr.moveUp();
		}
	}

	@Override
	public void enterCompDimMeasure(final CompDimMeasureContext ctx) {

		// add the function to the tree
		if (ctx.selectorSecondMathOperator() != null) {
			final ArithmeticOperator ao = resolveArithmeticOperator(ctx
					.selectorSecondMathOperator());
			mathExpr.attach(ao);
		}
	}

	@Override
	public void exitCompDimMeasure(final CompDimMeasureContext ctx) {

		// check if we had an operator and we have to move up
		if (ctx.selectorSecondMathOperator() != null) {
			mathExpr.moveUp();
		}
	}

	@Override
	public void enterCompDimMeasureAtom(final CompDimMeasureAtomContext ctx) {
		if (ctx.selectorFirstMathOperator() != null) {
			final ArithmeticOperator ao = resolveArithmeticOperator(ctx
					.selectorFirstMathOperator());
			mathExpr.attach(ao);
		} else if (ctx.compDimAggrFunction() != null) {
			final CompDimAggrFunctionContext aggFuncCtx = ctx
					.compDimAggrFunction();
			mathExpr.attach(resolveAggregationFunction(
					aggFuncCtx.selectorDimAggrFunctionName(),
					IDimAggregationFunction.class));
		}
	}

	@Override
	public void exitCompDimMeasureAtom(final CompDimMeasureAtomContext ctx) {
		if (ctx.selectorFirstMathOperator() != null) {
			mathExpr.moveUp();
		} else if (ctx.compDimAggrFunction() != null) {
			mathExpr.moveUp();
		}
	}

	@Override
	public void enterCompMathMeasure(final CompMathMeasureContext ctx) {

		// add the function to the tree
		if (ctx.selectorSecondMathOperator() != null) {
			final ArithmeticOperator ao = resolveArithmeticOperator(ctx
					.selectorSecondMathOperator());
			mathExpr.attach(ao);
		}
	}

	@Override
	public void exitCompMathMeasure(final CompMathMeasureContext ctx) {

		// check if we had an operator and we have to move up
		if (ctx.selectorSecondMathOperator() != null) {
			mathExpr.moveUp();
		}
	}

	@Override
	public void enterCompMathMeasureAtom(final CompMathMeasureAtomContext ctx) {
		if (ctx.selectorFirstMathOperator() != null) {
			final ArithmeticOperator ao = resolveArithmeticOperator(ctx
					.selectorFirstMathOperator());
			mathExpr.attach(ao);
		} else if (ctx.compMathAggrFunction() != null) {
			final CompMathAggrFunctionContext aggFuncCtx = ctx
					.compMathAggrFunction();
			mathExpr.attach(resolveAggregationFunction(
					aggFuncCtx.selectorMathAggrFunctionName(),
					IMathAggregationFunction.class));
		}
	}

	@Override
	public void exitCompMathMeasureAtom(final CompMathMeasureAtomContext ctx) {
		if (ctx.selectorFirstMathOperator() != null) {
			mathExpr.moveUp();
		} else if (ctx.compMathAggrFunction() != null) {
			mathExpr.moveUp();
		}
	}

	@Override
	public void enterCompDescriptorFormula(
			final CompDescriptorFormulaContext ctx) {
		if (ctx.selectorSecondMathOperator() != null) {
			final ArithmeticOperator ao = resolveArithmeticOperator(ctx
					.selectorSecondMathOperator());
			mathExpr.attach(ao);
		}
	}

	@Override
	public void exitCompDescriptorFormula(final CompDescriptorFormulaContext ctx) {
		if (ctx.selectorSecondMathOperator() != null) {
			mathExpr.moveUp();
		}
	}

	@Override
	public void enterCompDescriptorFormulaAtom(
			final CompDescriptorFormulaAtomContext ctx) {
		if (ctx.selectorFirstMathOperator() != null) {
			final ArithmeticOperator ao = resolveArithmeticOperator(ctx
					.selectorFirstMathOperator());
			mathExpr.attach(ao);
		} else if (ctx.selectorDescriptorId() != null) {
			mathExpr.attach(ctx.selectorDescriptorId().getText());
		}
	}

	@Override
	public void exitCompDescriptorFormulaAtom(
			final CompDescriptorFormulaAtomContext ctx) {
		if (ctx.selectorFirstMathOperator() != null) {
			mathExpr.moveUp();
		}
	}

	@Override
	public void exitExprMeasure(final ExprMeasureContext ctx) {
		if (ctx.selectorMember() != null) {
			final DimensionSelector dimSel = getMember(ctx.selectorMember());
			q(SelectQuery.class).setMeasureDimension(dimSel);
		}
	}

	@Override
	public void enterExprComp(final ExprCompContext ctx) {
		final LogicalOperator op = resolveLogicalOperator(ctx);

		if (op == null) {
			// do nothing
		} else {
			q(SelectQuery.class).getFilter().attach(op);
		}
	}

	@Override
	public void exitExprComp(final ExprCompContext ctx) {

		// if we reached the parent logic we don't have to move
		if (ctx.getParent() instanceof ExprSelectContext) {
			return;
		}

		// check the not
		final LogicalOperator op = resolveLogicalOperator(ctx);
		if (op == null) {
			// do nothing
		} else {
			q(SelectQuery.class).getFilter().moveUp();
		}
	}

	@Override
	public void exitCompDescriptorEqual(final CompDescriptorEqualContext ctx) {
		final String id = getDescriptorModelId(ctx.selectorDescriptorId());
		final String value = getSelectorValue(ctx.selectorValue());

		final DescriptorComperator descCmp = new DescriptorComperator(id, value);
		q(SelectQuery.class).getFilter().attach(descCmp);
	}

	@Override
	public void exitCompMemberEqual(final CompMemberEqualContext ctx) {

		final DimensionSelector dimSel = getMember(ctx.selectorMember());
		final String value = getSelectorValue(ctx.selectorValue());

		final DimensionComperator dimCmp = new DimensionComperator(dimSel,
				value);
		q(SelectQuery.class).getFilter().attach(dimCmp);
	}

	@Override
	public void exitExprGroup(final ExprGroupContext ctx) {

		// validate the created group
		if (!q(SelectQuery.class).getGroup().isValid()) {
			throw new ForwardedRuntimeException(QueryParsingException.class,
					1008, ctx.getText());
		}
	}

	@Override
	public void exitCompGroupInclude(final CompGroupIncludeContext ctx) {
		final GroupExpression groupExpr = q(SelectQuery.class).getGroup();
		for (final CompDescValueTupelContext descValueCtx : ctx
				.compGroupFilter().compDescValueTupel()) {

			// add an exclusion
			groupExpr.addInclusion(getDescriptorValueList(descValueCtx));
		}
	}

	@Override
	public void exitCompGroupExclude(final CompGroupExcludeContext ctx) {
		final GroupExpression groupExpr = q(SelectQuery.class).getGroup();
		for (final CompDescValueTupelContext descValueCtx : ctx
				.compGroupFilter().compDescValueTupel()) {

			// add an exclusion
			groupExpr.addExclusion(getDescriptorValueList(descValueCtx));
		}
	}

	@Override
	public void exitExprInterval(final ExprIntervalContext ctx) {

		// determine the types of the interval
		final IntervalType openType = resolveIntervalType(ctx
				.selectorOpenInterval());
		final IntervalType closeType = resolveIntervalType(ctx
				.selectorCloseInterval());

		// determine the values
		final SelectorDateIntervalContext dateCtx = ctx.selectorDateInterval();
		final SelectorIntIntervalContext intCtx = ctx.selectorIntInterval();
		final Interval<?> interval = resolveInterval(ctx.getText(), dateCtx,
				intCtx, openType, closeType);

		q(SelectQuery.class).setInterval(interval);
	}

	@Override
	public void exitExprSelectTimeSeries(final ExprSelectTimeSeriesContext ctx) {
		final SelectResultType type = resolveSelectResultType(ctx);
		final boolean transposed = resolveTransposition(ctx);

		q(SelectQuery.class).setResultType(type);
		q(SelectQuery.class).setTransposed(transposed);
	}

	@Override
	public void exitExprSelectRecords(final ExprSelectRecordsContext ctx) {
		final SelectResultType type = resolveSelectResultType(ctx);
		final boolean idsOnly = resolveIdsOnly(ctx);
		final boolean count = resolveCount(ctx);

		q(SelectQuery.class).setResultType(type);
		q(SelectQuery.class).setIdsOnly(idsOnly);
		q(SelectQuery.class).setCount(count);
	}

	@Override
	public void exitSelectorLimit(final SelectorLimitContext ctx) {
		q(SelectQuery.class).setLimit(Integer.parseInt(ctx.INT().getText()));
	}

	@Override
	public void exitSelectorOffset(final SelectorOffsetContext ctx) {
		q(SelectQuery.class).setOffset(Integer.parseInt(ctx.INT().getText()));
	}

	@Override
	public void exitSelectorModelId(final SelectorModelIdContext ctx) {
		q(IQuery.class).setModelId(getModelId(ctx));
	}

	@Override
	public void exitSelectorIntIdList(final SelectorIntIdListContext ctx) {
		final List<TerminalNode> intNodes = ctx.INT();
		final int[] idList = new int[intNodes.size()];

		for (int i = 0; i < intNodes.size(); i++) {
			idList[i] = Integer.parseInt(intNodes.get(i).getText());
		}

		q(DeleteQuery.class).setIdList(idList);
	}

	@Override
	public void enterExprAggregate(
			final QueryGrammarParser.ExprAggregateContext ctx) {
	}

	@Override
	public void exitSelectorIntervalRelation(
			final SelectorIntervalRelationContext ctx) {
		final IntervalRelation relation = resolveIntervalRelation(ctx);

		q(SelectQuery.class).setIntervalRelation(relation);
	}

	@Override
	public void exitExprAggregate(final ExprAggregateContext ctx) {
		final List<Object> selectors = new ArrayList<Object>();

		// get all the defined identifiers
		final int childrenSize = ctx.getChildCount();
		for (int i = 0; i < childrenSize; i += 2) {
			final ParseTree child = ctx.getChild(i);

			final Object value;
			if (child instanceof SelectorDescriptorIdContext) {
				value = getDescriptorModelId((SelectorDescriptorIdContext) child);
			} else if (child instanceof SelectorMemberContext) {
				value = getMember((SelectorMemberContext) child);
			} else {
				throw new ForwardedRuntimeException(
						QueryParsingException.class, 1023, child == null ? null
								: child.getClass().getSimpleName());
			}

			selectors.add(value);
		}

		// set the retrieved identifiers
		q(SelectQuery.class).getGroup().setSelectors(selectors);
	}

	/**
	 * Gets the {@code query} casted to the needed return type.
	 * 
	 * @return the {@code query} casted to the needed return type
	 */
	@SuppressWarnings("unchecked")
	protected <T extends IQuery> T q() {
		if (query == null) {
			throw new ForwardedRuntimeException(QueryParsingException.class,
					1006);
		}

		return (T) query;
	}

	/**
	 * Gets the {@code query} casted to the specified {@code clazz}.
	 * 
	 * @param clazz
	 *            the class to cast the current {@link IQuery} to
	 * 
	 * @return the casted type
	 */
	@SuppressWarnings("unchecked")
	protected <T extends IQuery> T q(final Class<T> clazz) {
		return (T) q();
	}

	/**
	 * Gets the query which was parsed.
	 * 
	 * @return the parsed {@code IQuery}
	 */
	public IQuery getQuery() {
		if (!finalized) {
			throw new ForwardedRuntimeException(QueryParsingException.class,
					1007);
		}

		return query;
	}

	/**
	 * Gets the optimization setting, i.e. {@code true} is returned if the query
	 * is optimized, otherwise {@code false}.
	 * 
	 * @return the optimization setting, i.e. {@code true} is returned if the
	 *         query is optimized, otherwise {@code false}
	 */
	public boolean isOptimize() {
		return optimize;
	}

	/**
	 * Gets the list of defined descriptor-value-pairs.
	 * 
	 * @param descValueCtx
	 *            the context to retrieve the pairs from
	 * 
	 * @return the retrieved pairs
	 */
	protected List<String> getDescriptorValueList(
			final CompDescValueTupelContext descValueCtx) {

		final List<String> values = new ArrayList<String>();
		for (final SelectorValueContext selectorDesc : descValueCtx
				.selectorValue()) {
			values.add(getSelectorValue(selectorDesc));
		}

		return values;
	}

	/**
	 * Gets the defined descriptor value from the parsed string, i.e.
	 * {@code 'value' => value} or {@code 'v\\al\'ue' => v\al'ue}.
	 * 
	 * @param selectorCtx
	 *            the text to retrieve the descriptor value for
	 * @return the descriptors value
	 */
	protected String getSelectorValue(final SelectorValueContext selectorCtx) {
		if (selectorCtx.NULL_VALUE() != null) {
			return null;
		} else {

			// get the value the descriptor should have
			return getValue(selectorCtx.VALUE());
		}
	}

	/**
	 * Gets the {@code DefinedPermissions} within the
	 * {@code SelectorValueListContext}.
	 * 
	 * @param selectorValueList
	 *            the context to get the {@code DefinedPermissions} from
	 * 
	 * @return the {@code DefinedPermissions}
	 * 
	 * @see DefinedPermission
	 */
	protected List<DefinedPermission> getPermissions(
			final SelectorValueListContext selectorValueList) {
		final List<String> permissions = getValues(selectorValueList);
		final List<DefinedPermission> perms = new ArrayList<DefinedPermission>();
		for (final String permission : permissions) {
			try {
				final DefinedPermission perm = DefinedPermission
						.fromString(permission);
				perms.add(perm);
			} catch (final IllegalArgumentException e) {
				throw new ForwardedRuntimeException(
						QueryParsingException.class, 1021, permission);
			}
		}

		return perms;
	}

	/**
	 * Gets the defined values within the {@code SelectorValueListContext}.
	 * 
	 * @param selectorValueList
	 *            the context to get the defined values from
	 * 
	 * @return the defined values
	 */
	protected List<String> getValues(
			final SelectorValueListContext selectorValueList) {
		final List<String> values = new ArrayList<String>();

		final List<TerminalNode> valueNodes = selectorValueList.VALUE();
		for (final TerminalNode valueNode : valueNodes) {
			values.add(getValue(valueNode));
		}

		return values;
	}

	/**
	 * Gets the value, i.e. {@link TerminalNode#getText()} is used to determine
	 * the value, and removes quotes.
	 * 
	 * @param node
	 *            the node to determine the value from
	 * 
	 * @return the value
	 */
	protected String getValue(final TerminalNode node) {
		final String value = node.getText();
		return Strings.trimSequence(value, "'").replace("\\'", "'")
				.replace("\\\\", "\\");
	}

	/**
	 * Gets the identifier defined by the specified
	 * {@code SelectorModelIdContext}.
	 * 
	 * @param ctx
	 *            the context to retrieve the identifier from
	 * 
	 * @return the identifier defined by the {@code SelectorModelIdContext}
	 */
	protected String getModelId(final SelectorModelIdContext ctx) {
		if (ctx.SIMPLE_ID() != null) {
			return ctx.SIMPLE_ID().getText();
		} else if (ctx.ENHANCED_ID() != null) {
			return ctx.ENHANCED_ID().getText();
		} else {
			return Strings.trimSequence(ctx.MARKED_ID().getText(), "\"");
		}
	}

	/**
	 * Gets the identifier defined by the specified
	 * {@code SelectorDescriptorIdContext}.
	 * 
	 * @param ctx
	 *            the context to retrieve the identifier from
	 * 
	 * @return the identifier defined by the {@code SelectorDescriptorIdContext}
	 */
	protected String getDescriptorModelId(final SelectorDescriptorIdContext ctx) {
		if (ctx.SIMPLE_ID() != null) {
			return ctx.SIMPLE_ID().getText();
		} else if (ctx.ENHANCED_ID() != null) {
			return ctx.ENHANCED_ID().getText();
		} else {
			return Strings.trimSequence(ctx.MARKED_ID().getText(), "\"");
		}
	}

	/**
	 * Gets the member defined within the {@code SelectorMemberContext}.
	 * 
	 * @param selMember
	 *            the context to read the member from
	 * 
	 * @return the {@code DimensionSelector} defined by the context
	 */
	protected DimensionSelector getMember(final SelectorMemberContext selMember) {
		final int childrenSize = selMember.getChildCount();

		if (childrenSize != 5) {
			throw new ForwardedRuntimeException(QueryParsingException.class,
					1022, selMember.getText());
		}

		final String dimId = selMember.getChild(TerminalNode.class, 0)
				.getText();
		final String hierarchyId = selMember.getChild(TerminalNode.class, 2)
				.getText();
		final String levelId = selMember.getChild(TerminalNode.class, 4)
				.getText();

		// make sure none is empty
		if (Objects.empty(dimId) || Objects.empty(hierarchyId)
				|| Objects.empty(levelId)) {
			throw new ForwardedRuntimeException(QueryParsingException.class,
					1022, selMember.getText());
		}

		return new DimensionSelector(dimId, hierarchyId, levelId);
	}

	/**
	 * Gets the alias defined by the specified {@code SelectorAliasContext}.
	 * 
	 * @param ctx
	 *            the context to retrieve the alias from
	 * 
	 * @return the alias defined by the {@code SelectorAliasContext}
	 */
	protected String getAlias(final SelectorAliasContext ctx) {
		if (ctx == null) {
			return UUID.randomUUID().toString();
		} else if (ctx.SIMPLE_ID() != null) {
			return ctx.SIMPLE_ID().getText();
		} else if (ctx.ENHANCED_ID() != null) {
			return ctx.ENHANCED_ID().getText();
		} else {
			return Strings.trimSequence(ctx.MARKED_ID().getText(), "\"");
		}
	}

	/**
	 * Resolves the name of the function to the concrete implementation of the
	 * function.
	 * 
	 * @param ctx
	 *            the name of the function
	 * @param expected
	 *            the expected type of the function
	 * 
	 * @return the instance of the {@code AggregationFunction}
	 * 
	 * @throws QueryParsingException
	 *             if the function cannot be resolved, more detailed the
	 *             {@code QueryParsingException} is wrapped within a
	 *             {@code ForwardedRuntimeException}
	 */
	@SuppressWarnings("unchecked")
	protected <D extends IAggregationFunction> D resolveAggregationFunction(
			final ParserRuleContext ctx, final Class<D> expected)
			throws QueryParsingException {
		if (aggFuncHandler == null) {
			throw new ForwardedRuntimeException(QueryParsingException.class,
					1010, ctx == null ? null : ctx.getText());
		}

		if (ctx instanceof SelectorDimAggrFunctionNameContext
				|| ctx instanceof SelectorLowAggrFunctionNameContext
				|| ctx instanceof SelectorMathAggrFunctionNameContext) {
			final String funcName = ctx.getText();
			final IAggregationFunction func = aggFuncHandler.resolve(funcName);
			if (func == null) {
				throw new ForwardedRuntimeException(
						QueryParsingException.class, 1009, funcName);
			} else if (expected != null
					&& !expected.isAssignableFrom(func.getClass())) {
				throw new ForwardedRuntimeException(
						QueryParsingException.class, 1024, funcName,
						expected.getSimpleName());
			} else {
				func.setDefinedType(expected);
				return (D) func;
			}
		} else {
			throw new IllegalArgumentException("The context '" + ctx
					+ "' does not contain any aggregation function.");
		}
	}

	/**
	 * Resolves the {@code LogicalOperator} based on the specified context.
	 * 
	 * @param ctx
	 *            the context used to resolve the
	 * 
	 * @return the resolved {@code LogicalOperator}, can be {@code null} if it
	 *         cannot be resolved
	 */
	protected LogicalOperator resolveLogicalOperator(final ParserRuleContext ctx) {

		if (ctx.getToken(QueryGrammarParser.LOGICAL_AND, 0) != null) {
			return LogicalOperator.AND;
		} else if (ctx.getToken(QueryGrammarParser.LOGICAL_OR, 0) != null) {
			return LogicalOperator.OR;
		} else if (ctx.getToken(QueryGrammarParser.LOGICAL_NOT, 0) != null) {
			return LogicalOperator.NOT;
		} else {
			return null;
		}
	}

	/**
	 * Resolves the {@code ArithmeticOperator} based on the specified context.
	 * 
	 * @param ctx
	 *            the context used to resolve the
	 * 
	 * @return the resolved {@code ArithmeticOperator}, can be {@code null} if
	 *         it cannot be resolved
	 */
	protected ArithmeticOperator resolveArithmeticOperator(
			final ParserRuleContext ctx) {
		if (ctx.getToken(QueryGrammarParser.MATH_PLUS, 0) != null) {
			return ArithmeticOperator.ADD;
		} else if (ctx.getToken(QueryGrammarParser.MATH_MINUS, 0) != null) {
			return ArithmeticOperator.MINUS;
		} else if (ctx.getToken(QueryGrammarParser.MATH_MULTIPLY, 0) != null) {
			return ArithmeticOperator.MULTIPLY;
		} else if (ctx.getToken(QueryGrammarParser.MATH_DIVISION, 0) != null) {
			return ArithmeticOperator.DIVIDE;
		} else {
			return null;
		}
	}

	/**
	 * Determine the type of the interval based on the passed context of the
	 * parser.
	 * 
	 * @param ctx
	 *            the context of the parser to be checked
	 * 
	 * @return the determined {@code IntervalType}
	 * 
	 * @throws QueryParsingException
	 *             if the {@code IntervalType} cannot be resolved, more detailed
	 *             the {@code QueryParsingException} is wrapped within a
	 *             {@code ForwardedRuntimeException}
	 */
	protected IntervalType resolveIntervalType(final ParserRuleContext ctx)
			throws QueryParsingException {
		if (ctx.getToken(QueryGrammarParser.BRACKET_ROUND_OPENED, 0) != null) {
			return IntervalType.EXCLUDE;
		} else if (ctx.getToken(QueryGrammarParser.BRACKET_ROUND_CLOSED, 0) != null) {
			return IntervalType.EXCLUDE;
		} else if (ctx.getToken(QueryGrammarParser.BRACKET_SQUARE_OPENED, 0) != null) {
			return IntervalType.INCLUDE;
		} else if (ctx.getToken(QueryGrammarParser.BRACKET_SQUARE_CLOSED, 0) != null) {
			return IntervalType.INCLUDE;
		} else {
			throw new ForwardedRuntimeException(QueryParsingException.class,
					1002, ctx.getText());
		}
	}

	/**
	 * Resolves the type of an interval for a {@code SelectorIntervalDefContext}
	 * .
	 * 
	 * @param ctx
	 *            the context to resolve the type from
	 * @return the resolved type
	 * 
	 * @see SelectorIntervalDefContext
	 */
	protected IntervalType resolveIntervalType(
			final SelectorIntervalDefContext ctx) {

		final IntervalType types;

		// determine the start
		if (ctx.POS_START_INCL() != null) {
			types = IntervalType.INCLUDE;
		} else if (ctx.POS_START_EXCL() != null) {
			types = IntervalType.EXCLUDE;
		} else if (ctx.POS_END_INCL() != null) {
			types = IntervalType.INCLUDE;
		} else if (ctx.POS_END_EXCL() != null) {
			types = IntervalType.EXCLUDE;
		} else {
			throw new ForwardedRuntimeException(QueryParsingException.class,
					1011, ctx.getText());
		}

		return types;
	}

	/**
	 * Determines if the {@code IntervalType} presented by the {@code ctx} (see
	 * {@link #resolveIntervalType(ParserRuleContext)}) is for the start (open)
	 * or the end (close).
	 * 
	 * @param ctx
	 *            the {@code SelectorIntervalDefContext} to determine the
	 *            position for
	 * 
	 * @return {@code true} if it's the start (open) position, otherwise
	 *         {@code false}
	 */
	protected boolean isStartIntervalType(final SelectorIntervalDefContext ctx) {
		final boolean start;

		// determine the start
		if (ctx.POS_START_INCL() != null) {
			start = true;
		} else if (ctx.POS_START_EXCL() != null) {
			start = true;
		} else if (ctx.POS_END_INCL() != null) {
			start = false;
		} else if (ctx.POS_END_EXCL() != null) {
			start = false;
		} else {
			throw new ForwardedRuntimeException(QueryParsingException.class,
					1011, ctx.getText());
		}

		return start;
	}

	/**
	 * Determines the {@code SelectResultType} from the context.
	 * 
	 * @param ctx
	 *            the context of the parser to be checked
	 * 
	 * @return the resolved {@code SelectResultType}
	 * 
	 * @throws QueryParsingException
	 *             if the {@code SelectResultType} cannot be resolved, more
	 *             detailed the {@code QueryParsingException} is wrapped within
	 *             a {@code ForwardedRuntimeException}
	 */
	protected SelectResultType resolveSelectResultType(
			final ParserRuleContext ctx) throws QueryParsingException {

		if (ctx.getToken(QueryGrammarParser.TYPE_RECORDS, 0) != null) {
			return SelectResultType.RECORDS;
		} else if (ctx.getToken(QueryGrammarParser.TYPE_TIMESERIES, 0) != null) {
			return SelectResultType.TIMESERIES;
		} else {
			throw new ForwardedRuntimeException(QueryParsingException.class,
					1005, ctx.getText());
		}
	}

	/**
	 * Determines the {@code GetResultType} from the context.
	 * 
	 * @param ctx
	 *            the context of the parser to be checked
	 * 
	 * @return the resolved {@code GetResultType}
	 * 
	 * @throws QueryParsingException
	 *             if the {@code GetResultType} cannot be resolved, more
	 *             detailed the {@code QueryParsingException} is wrapped within
	 *             a {@code ForwardedRuntimeException}
	 */
	protected GetResultType resolveGetResultType(final ParserRuleContext ctx)
			throws QueryParsingException {

		if (ctx.getToken(QueryGrammarParser.TYPE_MODELS, 0) != null) {
			return GetResultType.MODELS;
		} else if (ctx.getToken(QueryGrammarParser.TYPE_VERSION, 0) != null) {
			return GetResultType.VERSION;
		} else if (ctx.getToken(QueryGrammarParser.TYPE_USERS, 0) != null) {
			return GetResultType.USERS;
		} else if (ctx.getToken(QueryGrammarParser.TYPE_PERMISSIONS, 0) != null) {
			return GetResultType.PERMISSIONS;
		} else if (ctx.getToken(QueryGrammarParser.TYPE_ROLES, 0) != null) {
			return GetResultType.ROLES;
		} else {
			throw new ForwardedRuntimeException(QueryParsingException.class,
					1005, ctx.getText());
		}
	}

	private IntervalRelation resolveIntervalRelation(
			final SelectorIntervalRelationContext ctx) {
		if (ctx.getToken(QueryGrammarParser.IR_WITHIN, 0) != null) {
			return IntervalRelation.WITHIN;
		} else if (ctx.getToken(QueryGrammarParser.IR_EQUALTO, 0) != null) {
			return IntervalRelation.EQUALTO;
		} else if (ctx.getToken(QueryGrammarParser.IR_BEFORE, 0) != null) {
			return IntervalRelation.BEFORE;
		} else if (ctx.getToken(QueryGrammarParser.IR_AFTER, 0) != null) {
			return IntervalRelation.AFTER;
		} else if (ctx.getToken(QueryGrammarParser.IR_MEETING, 0) != null) {
			return IntervalRelation.MEETING;
		} else if (ctx.getToken(QueryGrammarParser.IR_OVERLAPPING, 0) != null) {
			return IntervalRelation.OVERLAPPING;
		} else if (ctx.getToken(QueryGrammarParser.IR_DURING, 0) != null) {
			return IntervalRelation.DURING;
		} else if (ctx.getToken(QueryGrammarParser.IR_CONTAINING, 0) != null) {
			return IntervalRelation.CONTAINING;
		} else if (ctx.getToken(QueryGrammarParser.IR_STARTINGWITH, 0) != null) {
			return IntervalRelation.STARTINGWITH;
		} else if (ctx.getToken(QueryGrammarParser.IR_FINISHINGWITH, 0) != null) {
			return IntervalRelation.FINISHINGWITH;
		} else {
			throw new ForwardedRuntimeException(QueryParsingException.class,
					1019, ctx.getText());
		}
	}

	/**
	 * Checks if the result should be transposed.
	 * 
	 * @param ctx
	 *            the context of the parser to be checked
	 * 
	 * @return {@code true} if the result should be transposed, otherwise
	 *         {@code false}
	 */
	protected boolean resolveTransposition(final ParserRuleContext ctx) {
		if (ctx.getToken(QueryGrammarParser.OP_TRANSPOSE, 0) != null) {
			return true;
		} else {
			return false;
		}
	}

	/**
	 * Checks if the result should be counted.
	 * 
	 * @param ctx
	 *            the context of the parser to be checked
	 * 
	 * @return {@code true} if the result should be counted, otherwise
	 *         {@code false}
	 */
	protected boolean resolveCount(final ParserRuleContext ctx) {
		if (ctx.getToken(QueryGrammarParser.AGGR_COUNT, 0) != null) {
			return true;
		} else {
			return false;
		}
	}

	/**
	 * Checks if the result should be the identifiers only.
	 * 
	 * @param ctx
	 *            the context of the parser to be checked
	 * 
	 * @return {@code true} if the result should be the identifiers only,
	 *         otherwise {@code false}
	 */
	protected boolean resolveIdsOnly(final ParserRuleContext ctx) {
		if (ctx.getToken(QueryGrammarParser.OP_IDONLY, 0) != null) {
			return true;
		} else {
			return false;
		}
	}

	/**
	 * Resolves a value of a {@code CompValueElementContext}, a
	 * {@code SelectorDateValueContext}, {@code SelectorIntValueContext},
	 * {@code SelectorNullValueContext}, or a {@code SelectorValueContext}.
	 * 
	 * @param ctx
	 *            the context to read the value from
	 * 
	 * @return the value read
	 */
	protected Object resolveValue(final ParserRuleContext ctx) {

		if (ctx instanceof CompValueElementContext) {
			final CompValueElementContext c = (CompValueElementContext) ctx;

			if (c.selectorDateValue() != null) {
				return resolveValue(c.selectorDateValue());
			} else if (c.selectorIntValue() != null) {
				return resolveValue(c.selectorIntValue());
			} else if (c.selectorNullValue() != null) {
				return resolveValue(c.selectorNullValue());
			} else if (c.selectorValue() != null) {
				return resolveValue(c.selectorValue());
			} else {
				throw new ForwardedRuntimeException(
						QueryParsingException.class, 1015, c.getText());
			}
		} else if (ctx instanceof SelectorDateValueContext) {
			final SelectorDateValueContext c = (SelectorDateValueContext) ctx;
			if (c.DATE() != null) {
				return Dates.isDate(c.DATE().getText(), Dates.GENERAL_TIMEZONE);
			} else {
				throw new ForwardedRuntimeException(
						QueryParsingException.class, 1015, c.getText());
			}
		} else if (ctx instanceof SelectorIntValueContext) {
			final SelectorIntValueContext c = (SelectorIntValueContext) ctx;

			if (c.INT() != null) {
				return Long.parseLong(c.INT().getText());
			} else {
				throw new ForwardedRuntimeException(
						QueryParsingException.class, 1015, c.getText());
			}
		} else if (ctx instanceof SelectorNullValueContext) {
			final SelectorNullValueContext c = (SelectorNullValueContext) ctx;

			if (c.NULL_VALUE() != null) {
				return null;
			} else {
				throw new ForwardedRuntimeException(
						QueryParsingException.class, 1015, c.getText());
			}
		} else if (ctx instanceof SelectorValueContext) {
			return getSelectorValue((SelectorValueContext) ctx);
		} else {
			throw new ForwardedRuntimeException(QueryParsingException.class,
					1015, ctx.getText());
		}
	}

	/**
	 * Resolves the {@code Interval} from the specified
	 * {@code ParserRuleContext}. Currently only the
	 * {@code SelectorDateIntervalContext}, the
	 * {@code SelectorDateIntervalWithNullContext}, the
	 * {@code SelectorIntIntervalContext}, and the
	 * {@code SelectorIntIntervalWithNullContext} is supported.
	 * 
	 * @param ctxText
	 *            the text of the global context, used for error messages
	 * @param dateCtx
	 *            the date context
	 * @param intCtx
	 *            the int context
	 * @param openType
	 *            the type of the interval's start
	 * @param closeType
	 *            the type of the interval's end
	 * 
	 * @return the resolved interval
	 * 
	 * @see SelectorDateIntervalContext
	 * @see SelectorDateIntervalWithNullContext
	 * @see SelectorIntIntervalContext
	 * @see SelectorIntIntervalWithNullContext
	 */
	protected Interval<?> resolveInterval(final String ctxText,
			final ParserRuleContext dateCtx, final ParserRuleContext intCtx,
			final IntervalType openType, final IntervalType closeType) {

		// create the interval
		final Interval<?> interval;
		if (dateCtx != null) {
			final Date dates[] = new Date[2];

			// parse depending on the type
			if (dateCtx instanceof SelectorDateIntervalContext) {
				final SelectorDateIntervalContext c = (SelectorDateIntervalContext) dateCtx;

				dates[0] = Dates.isDate(c.DATE(0).getText(),
						Dates.GENERAL_TIMEZONE);
				dates[1] = Dates.isDate(c.DATE(1).getText(),
						Dates.GENERAL_TIMEZONE);
			} else if (dateCtx instanceof SelectorDateIntervalWithNullContext) {
				final SelectorDateIntervalWithNullContext c = (SelectorDateIntervalWithNullContext) dateCtx;

				int i = 0;
				for (ParseTree o : c.children) {
					if (o instanceof TerminalNode) {
						final Token symbol = ((TerminalNode) o).getSymbol();

						if (QueryGrammarParser.NULL_VALUE == symbol.getType()) {
							dates[i] = null;
							i++;
						} else if (QueryGrammarParser.DATE == symbol.getType()) {
							dates[i] = Dates.isDate(symbol.getText(),
									Dates.GENERAL_TIMEZONE);
							i++;
						}
					}
				}
			} else {
				throw new ForwardedRuntimeException(
						QueryParsingException.class, 1004, ctxText);
			}

			interval = new Interval<Date>(new DateIntervalValue(dates[0]),
					openType, new DateIntervalValue(dates[1]), closeType);
		} else if (intCtx != null) {
			final Long vals[] = new Long[2];

			// parse depending on the type
			if (intCtx instanceof SelectorIntIntervalContext) {
				final SelectorIntIntervalContext c = (SelectorIntIntervalContext) intCtx;

				vals[0] = Long.parseLong(c.INT(0).getText());
				vals[1] = Long.parseLong(c.INT(1).getText());
			} else if (intCtx instanceof SelectorIntIntervalWithNullContext) {
				final SelectorIntIntervalWithNullContext c = (SelectorIntIntervalWithNullContext) intCtx;

				int i = 0;
				for (ParseTree o : c.children) {
					if (o instanceof TerminalNode) {
						final Token symbol = ((TerminalNode) o).getSymbol();

						if (QueryGrammarParser.NULL_VALUE == symbol.getType()) {
							vals[i] = null;
							i++;
						} else if (QueryGrammarParser.INT == symbol.getType()) {
							vals[i] = Long.parseLong(symbol.getText());
							i++;
						}
					}
				}
			} else {
				throw new ForwardedRuntimeException(
						QueryParsingException.class, 1004, ctxText);
			}

			interval = new Interval<Long>(new LongIntervalValue(vals[0]),
					openType, new LongIntervalValue(vals[1]), closeType);
		} else {
			throw new ForwardedRuntimeException(QueryParsingException.class,
					1004, ctxText);
		}

		return interval;
	}

	/**
	 * This method is used to resolve a property value.
	 * 
	 * @param properties
	 *            the map to set the determined property value in
	 * @param propertyCtx
	 *            the context to read the property name from
	 * 
	 * @throws QueryParsingException
	 *             if the value cannot be resolved, more detailed the
	 *             {@code QueryParsingException} is wrapped within a
	 *             {@code ForwardedRuntimeException}
	 * 
	 */
	protected void resolveProperty(final Map<String, Object> properties,
			final ParserRuleContext propertyCtx) throws QueryParsingException {
		final int[] propKeys = new int[] { QueryGrammarParser.PROP_AUTOLOAD,
				QueryGrammarParser.PROP_FORCE };

		// find the property to be set
		TerminalNode token = null;
		for (int i = 0; i < propKeys.length; i++) {
			token = propertyCtx.getToken(propKeys[i], 0);
			if (token != null) {
				break;
			}
		}

		// couldn't find any token
		if (token == null) {
			throw new ForwardedRuntimeException(QueryParsingException.class,
					1020, propertyCtx.getText());
		}

		// get the value
		final String key = token.getText();
		final Object value = resolvePropertyValue(propertyCtx.getRuleContext(
				SelectorBooleanContext.class, 0));
		properties.put(key, value);
	}

	/**
	 * Resolves the value of a property defined within the specified
	 * {@code valueCtx}.
	 * 
	 * @param valueCtx
	 *            the context to retrieve the value from
	 * 
	 * @return the found value
	 */
	protected Object resolvePropertyValue(final ParserRuleContext valueCtx) {

		TerminalNode token;
		if ((token = valueCtx.getToken(QueryGrammarParser.LOGICAL_TRUE, 0)) != null) {
			return true;
		} else if ((token = valueCtx.getToken(QueryGrammarParser.LOGICAL_FALSE,
				0)) != null) {
			return false;
		} else if ((token = valueCtx.getToken(QueryGrammarParser.NULL_VALUE, 0)) != null) {
			return null;
		} else if ((token = valueCtx.getToken(QueryGrammarParser.INT, 0)) != null) {
			return Long.parseLong(token.getText());
		} else if ((token = valueCtx.getToken(QueryGrammarParser.DATE, 0)) != null) {
			return Dates.isDate(token.getText(), Dates.GENERAL_TIMEZONE);
		} else if ((token = valueCtx.getToken(QueryGrammarParser.VALUE, 0)) != null) {
			return getValue(token);
		} else {
			throw new ForwardedRuntimeException(QueryParsingException.class,
					1019, valueCtx.getText());
		}
	}
}
