package de.z0rdak.yawp.commands;

import com.mojang.brigadier.arguments.BoolArgumentType;
import com.mojang.brigadier.arguments.StringArgumentType;
import com.mojang.brigadier.builder.LiteralArgumentBuilder;
import de.z0rdak.yawp.commands.arguments.region.RegionArgumentType;
import de.z0rdak.yawp.core.flag.IFlag;
import de.z0rdak.yawp.core.flag.RegionFlag;
import de.z0rdak.yawp.core.region.IMarkableRegion;
import de.z0rdak.yawp.managers.data.region.RegionDataManager;
import de.z0rdak.yawp.util.MessageUtil;
import net.minecraft.command.CommandSource;
import net.minecraft.command.Commands;
import net.minecraft.command.ISuggestionProvider;
import net.minecraft.command.arguments.DimensionArgument;
import net.minecraft.util.text.TranslationTextComponent;

import java.util.Collections;
import java.util.Set;
import java.util.stream.Collectors;

import static de.z0rdak.yawp.commands.CommandConstants.*;
import static de.z0rdak.yawp.core.flag.FlagType.BOOLEAN_FLAG;
import static de.z0rdak.yawp.core.flag.FlagType.getFlagTypes;
import static de.z0rdak.yawp.util.CommandUtil.*;

/**
 * Management for flag values for List and Int Flags as well as default flag values
 */
public final class FlagCommands {

    public static final LiteralArgumentBuilder<CommandSource> FLAG_COMMAND = register();

    private FlagCommands(){}

    /**
     * /wp flag entity_spawning list add minecraft:zombie
     * /wp flag entity_spawning list remove minecraft:zombie*
     *
     * /wp flag <dim> <region> add|remove|enable|disable|invert
     *
     * <type> int|bool|list
     * /wp flag <dim> <region> <type> <flag> add|remove <entry>
     * /wp flag <dim> <region> <type> <flag> set <value>
     * /wp flag <dim> <region> <type> <flag> set interval|value <n>
     * /wp flag <dim> <region> <flag> info
     *
     * @return
     */
    private static LiteralArgumentBuilder<CommandSource> register() {
        Set<String> flagTypes = getFlagTypes();
        return literal(FLAG)
                .then(Commands.argument(DIM.toString(), DimensionArgument.dimension())
                        .then(Commands.argument(REGION.toString(), StringArgumentType.word())
                                .then(literal(ENABLE)
                                        .then(Commands.argument(ENABLE.toString(), BoolArgumentType.bool())
                                                .executes(ctx -> setActiveState(ctx.getSource(), getRegionArgument(ctx), getFlagArgument(ctx), BoolArgumentType.getBool(ctx, ENABLE.toString())))))
                                .then(literal(INVERT)
                                        .then(Commands.argument(INVERT.toString(), BoolArgumentType.bool())
                                                .executes(ctx -> setInvertState(ctx.getSource(), getRegionArgument(ctx), getFlagArgument(ctx), BoolArgumentType.getBool(ctx, INVERT.toString())))))

                                .suggests((ctx, builder) -> RegionArgumentType.region().listSuggestions(ctx, builder))
                                .then(Commands.argument(TYPE.toString(), StringArgumentType.word())
                                        .suggests((ctx, builder) -> ISuggestionProvider.suggest(Collections.singleton(BOOLEAN_FLAG.flagType), builder))
                                        .then(Commands.argument(FLAG.toString(), StringArgumentType.word())
                                                        .suggests((ctx, builder) -> ISuggestionProvider.suggest(
                                                                RegionFlag.getFlags(getFlagTypeArgument(ctx))
                                                                        .stream()
                                                                        .map(flag -> flag.name)
                                                                        .collect(Collectors.toSet()), builder)
                                                        )
                                                // .then(literal(ADD)
                                                //        .then(Commands.argument("entry", StringArgumentType.word())
                                                //                .suggests((ctx, builder) -> ISuggestionProvider.suggest(Collections.singleton("your-entry"), builder))
                                                //                .executes(ctx -> promptHelp(ctx.getSource()))))
                                                //.then(literal(REMOVE)
                                                //        .then(Commands.argument("entry", StringArgumentType.word())
                                                //                .suggests((ctx, builder) -> ISuggestionProvider.suggest(Collections.singleton("your-entry"), builder))
                                                //                .executes(ctx -> promptHelp(ctx.getSource()))))
                                        )
                                )
                        )
                );
    }

    public static int setActiveState(CommandSource src, IMarkableRegion region, RegionFlag flag, boolean enable) {
        IFlag regionFlag = region.getFlag(flag.name);
        if (regionFlag != null) {
            regionFlag.setIsActive(enable);
            region.updateFlag(regionFlag);
            RegionDataManager.save();
            MessageUtil.sendCmdFeedback(src, new TranslationTextComponent("Set flag '%s' active state to '%s'", flag.name, enable));
            return 0;
        }
        return 1;
    }

    public static int setInvertState(CommandSource src, IMarkableRegion region, RegionFlag flag, boolean invert) {
        IFlag regionFlag = region.getFlag(flag.name);
        if (regionFlag != null) {
            regionFlag.setInverted(invert);
            region.updateFlag(regionFlag);
            RegionDataManager.save();
            MessageUtil.sendCmdFeedback(src, new TranslationTextComponent("Set flag '%s' inverted state to '%s'", flag.name, invert));
            return 0;
        }
        return 1;
    }

    public static int promptBoolFlagInfo(CommandSource src, IMarkableRegion region, IFlag flag) {

        return 0;
    }

}
