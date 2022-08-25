package de.z0rdak.yawp.commands;

import com.mojang.brigadier.arguments.BoolArgumentType;
import com.mojang.brigadier.builder.LiteralArgumentBuilder;
import com.mojang.brigadier.exceptions.CommandSyntaxException;
import de.z0rdak.yawp.core.region.AbstractMarkableRegion;
import de.z0rdak.yawp.core.region.IMarkableRegion;
import de.z0rdak.yawp.core.stick.AbstractStick;
import de.z0rdak.yawp.core.stick.MarkerStick;
import de.z0rdak.yawp.managers.data.region.RegionDataManager;
import de.z0rdak.yawp.util.*;
import net.minecraft.command.CommandSource;
import net.minecraft.command.Commands;
import net.minecraft.command.arguments.DimensionArgument;
import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.item.ItemStack;
import net.minecraft.nbt.CompoundNBT;
import net.minecraft.util.RegistryKey;
import net.minecraft.world.World;

import java.util.ArrayList;
import java.util.Collection;

import static de.z0rdak.yawp.util.CommandUtil.*;
import static net.minecraft.command.ISuggestionProvider.suggest;

public class RegionCommands {

    public static final LiteralArgumentBuilder<CommandSource> REGION_COMMAND = registerRegionCommands();
    public static final LiteralArgumentBuilder<CommandSource> REGIONS_COMMAND = registerRegionsCommands();

    private RegionCommands() {
    }

    public static LiteralArgumentBuilder<CommandSource> registerRegionsCommands(){

        return CommandUtil.literal(CommandConstants.REGIONS);
    }

    // IDEA: Expand command for all area types
    // circle - expand radius
    // ...

    /**
     *
     */
    public static LiteralArgumentBuilder<CommandSource> registerRegionCommands() {
        return CommandUtil.literal(CommandConstants.REGION)
                .executes(ctx -> promptHelp(ctx.getSource()))
                .then(CommandUtil.literal(CommandConstants.HELP)
                        .executes(ctx -> promptHelp(ctx.getSource())));
    }

    private static int removeRegion(CommandSource source, String regionName, RegistryKey<World> dim) {
        return 0;
    }

    private static int info(CommandSource source, String regionName) {

        return 0;
    }

    private static int setActiveStates(CommandSource source, RegistryKey<World> dim, boolean activate) {
        try {
            // TODO: Handle errors and give feedback to player
            PlayerEntity player = source.getPlayerOrException();
            Collection<IMarkableRegion> regionsToProcess = RegionDataManager.get().getRegionsFor(dim);
            RegionDataManager.get().setActiveState(regionsToProcess, activate);
        } catch (CommandSyntaxException e) {
            e.printStackTrace();
        }
        return 0;
    }

    private static int setActiveState(CommandSource source, String regionName, RegistryKey<World> dim, boolean activate) {
        try {
            // TODO: Handle errors and give feedback to player
            PlayerEntity player = source.getPlayerOrException();
            Collection<IMarkableRegion> regionsToProcess = new ArrayList<>();
            IMarkableRegion region = RegionDataManager.get().getRegionIn(regionName, dim);
            if (region != null) {
                regionsToProcess.add(region);
            }
            RegionDataManager.get().setActiveState(regionsToProcess, activate);
        } catch (CommandSyntaxException e) {
            e.printStackTrace();
        }
        return 0;
    }

    private static int setAlertState(CommandSource source, String regionName, boolean mute) {

        return 0;
    }

    private static int promptHelp(CommandSource src) {
        MessageUtil.sendCmdFeedback(src, MessageUtil.buildHelpHeader("help.region.header"));
        MessageUtil.sendCmdFeedback(src, MessageUtil.buildHelpSuggestionLink("help.region.1", CommandConstants.REGION, CommandConstants.CREATE));
        MessageUtil.sendCmdFeedback(src, MessageUtil.buildHelpSuggestionLink("help.region.2", CommandConstants.REGION, CommandConstants.UPDATE));
        MessageUtil.sendCmdFeedback(src, MessageUtil.buildHelpSuggestionLink("help.region.3", CommandConstants.REGION, CommandConstants.REMOVE));
        MessageUtil.sendCmdFeedback(src, MessageUtil.buildHelpSuggestionLink("help.region.4", CommandConstants.REGION, CommandConstants.LIST));
        MessageUtil.sendCmdFeedback(src, MessageUtil.buildHelpSuggestionLink("help.region.5", CommandConstants.REGION, CommandConstants.INFO));
        MessageUtil.sendCmdFeedback(src, MessageUtil.buildHelpSuggestionLink("help.region.6", CommandConstants.REGION, CommandConstants.PRIORITY));
        MessageUtil.sendCmdFeedback(src, MessageUtil.buildHelpSuggestionLink("help.region.7", CommandConstants.REGION, CommandConstants.TELEPORT));
        MessageUtil.sendCmdFeedback(src, MessageUtil.buildHelpSuggestionLink("help.region.8", CommandConstants.REGION, CommandConstants.DEACTIVATE));
        MessageUtil.sendCmdFeedback(src, MessageUtil.buildHelpSuggestionLink("help.region.9", CommandConstants.REGION,  CommandConstants.ALERT));
        return 0;
    }

    private static int promptRegionList(CommandSource source) {
        try {
            promptRegionListForDim(source, source.getPlayerOrException().getCommandSenderWorld().dimension());
        } catch (CommandSyntaxException e) {
            e.printStackTrace();
        }
        return 0;
    }

    private static int promptRegionListForDim(CommandSource source, RegistryKey<World> dim) {

        return 0;
    }

    private static int createRegion(CommandSource source, String regionName, RegistryKey<World> dim) {
        try {
            createRegion(source, regionName, dim, source.getPlayerOrException());
        } catch (CommandSyntaxException e) {
            e.printStackTrace();
        }
        return 0;
    }

    private static int createRegion(CommandSource source, String regionName, RegistryKey<World> dim, PlayerEntity owner) {
        try {
            PlayerEntity player = source.getPlayerOrException();
            ItemStack maybeStick = player.getMainHandItem();
            // TODO: create a method which trhows exception on trying to get stick
            if (StickUtil.isVanillaStick(maybeStick)) {
                StickType stickType = StickUtil.getStickType(maybeStick);
                if (stickType == StickType.MARKER) {
                    CompoundNBT stickNBT = StickUtil.getStickNBT(maybeStick);
                    if (stickNBT != null) {
                        AbstractMarkableRegion region = RegionUtil.regionFrom(source.getPlayerOrException(), new MarkerStick(stickNBT), regionName);
                        // TODO
                    }
                }
            }
        } catch (CommandSyntaxException e) {
            CommandUtil.handleCommandWithoutPlayer(e);
        }
        return 0;
    }

    // assumption: regions are only updated with the region marker when in the same dimension
    private static int updateRegion(CommandSource source, String regionName) {
        try {
            PlayerEntity player = source.getPlayerOrException();
            ItemStack maybeStick = player.getMainHandItem();
            if (StickUtil.isVanillaStick(maybeStick)) {
                try {
                    AbstractStick abstractStick = StickUtil.getStick(maybeStick);
                    if (abstractStick.getStickType() == StickType.MARKER){
                        MarkerStick marker = (MarkerStick) abstractStick;
                        // TODO:
                        //RegionDataManager.get().update(regionName, marker);
                    }
                } catch (StickException e) {
                    e.printStackTrace();
                }
            }
        } catch (CommandSyntaxException e) {
            e.printStackTrace();
        }
        return 0;
    }

    private static int removeRegions(CommandSource source, RegistryKey<World> dim) {
        try {
            //
            PlayerEntity player = source.getPlayerOrException();
        } catch (CommandSyntaxException e) {
            e.printStackTrace();
        }
        return 0;
    }

    private static int listRegionsAround(CommandSource source) {

        return 0;
    }

    private static int teleport(CommandSource source, String regionName, RegistryKey<World> dim) {
        // use execute in dim for tp because of different dimensions
        return 0;
    }

    private static int setPriority(CommandSource source, String region, int priority) {

        return 0;
    }
}
