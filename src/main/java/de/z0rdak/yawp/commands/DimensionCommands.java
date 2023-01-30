package de.z0rdak.yawp.commands;

import com.mojang.brigadier.arguments.BoolArgumentType;
import com.mojang.brigadier.arguments.StringArgumentType;
import com.mojang.brigadier.builder.LiteralArgumentBuilder;
import de.z0rdak.yawp.commands.arguments.region.RegionArgumentType;
import de.z0rdak.yawp.config.server.CommandPermissionConfig;
import de.z0rdak.yawp.config.server.RegionConfig;
import de.z0rdak.yawp.core.affiliation.PlayerContainer;
import de.z0rdak.yawp.core.area.AreaType;
import de.z0rdak.yawp.core.area.CuboidArea;
import de.z0rdak.yawp.core.area.SphereArea;
import de.z0rdak.yawp.core.flag.BooleanFlag;
import de.z0rdak.yawp.core.flag.IFlag;
import de.z0rdak.yawp.core.flag.RegionFlag;
import de.z0rdak.yawp.core.region.*;
import de.z0rdak.yawp.managers.data.region.DimensionRegionCache;
import de.z0rdak.yawp.managers.data.region.RegionDataManager;
import de.z0rdak.yawp.util.LocalRegions;
import de.z0rdak.yawp.util.MessageUtil;
import net.minecraft.ChatFormatting;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.commands.Commands;
import net.minecraft.commands.SharedSuggestionProvider;
import net.minecraft.commands.arguments.DimensionArgument;
import net.minecraft.commands.arguments.EntityArgument;
import net.minecraft.commands.arguments.TeamArgument;
import net.minecraft.commands.arguments.coordinates.BlockPosArgument;
import net.minecraft.core.BlockPos;
import net.minecraft.nbt.NbtUtils;
import net.minecraft.network.chat.ClickEvent;
import net.minecraft.network.chat.MutableComponent;
import net.minecraft.network.chat.TextComponent;
import net.minecraft.network.chat.TranslatableComponent;
import net.minecraft.resources.ResourceKey;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.level.Level;
import net.minecraft.world.scores.PlayerTeam;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import java.util.*;
import java.util.stream.Collectors;

import static de.z0rdak.yawp.commands.CommandConstants.*;
import static de.z0rdak.yawp.util.CommandUtil.*;
import static de.z0rdak.yawp.util.MessageUtil.*;
import static net.minecraft.ChatFormatting.RESET;
import static net.minecraft.ChatFormatting.*;

public class DimensionCommands {

    private DimensionCommands() {
    }

    public static final LiteralArgumentBuilder<CommandSourceStack> DIMENSION_COMMAND = register();

    public static LiteralArgumentBuilder<CommandSourceStack> register() {
        List<String> affiliationList = Arrays.asList("member", "owner");
        return literal(DIMENSION)
                /* /wp dimension <dim> list region */
                .then(Commands.argument(DIMENSION.toString(), DimensionArgument.dimension())
                        .then(literal(CREATE)
                                .then(literal(REGION)
                                        .then(Commands.argument(REGION.toString(), StringArgumentType.word())
                                                .suggests((ctx, builder) -> SharedSuggestionProvider.suggest(Collections.singletonList("name"), builder))
                                                //.then(Commands.argument(AREA.toString(), StringArgumentType.word())
                                                //        .suggests((ctx, builder) -> AreaArgumentType.areaType().listSuggestions(ctx, builder))
                                                //        .executes(ctx -> createRegion(ctx.getSource(), getRegionNameArgument(ctx), getDimCacheArgument(ctx), getAreaTypeArgument(ctx))))
                                                .then(Commands.literal(AreaType.CUBOID.areaType)
                                                        .then(Commands.argument("pos1", BlockPosArgument.blockPos())
                                                                .then(Commands.argument("pos2", BlockPosArgument.blockPos())
                                                                        .executes(ctx -> createCuboidRegion(ctx.getSource(), getRegionNameArgument(ctx), getDimCacheArgument(ctx),
                                                                                BlockPosArgument.getSpawnablePos(ctx, "pos1"),
                                                                                BlockPosArgument.getSpawnablePos(ctx, "pos2"), null))
                                                                        .then(Commands.argument(OWNER.toString(), EntityArgument.player())
                                                                                .executes(ctx -> createCuboidRegion(ctx.getSource(), getRegionNameArgument(ctx), getDimCacheArgument(ctx),
                                                                                        BlockPosArgument.getSpawnablePos(ctx, "pos1"),
                                                                                        BlockPosArgument.getSpawnablePos(ctx, "pos2"), getOwnerArgument(ctx))))))
                                                )
                                        )
                                )
                        )
                        /* /wp dimension <dim> [info] */
                        .executes(ctx -> promptDimensionInfo(ctx.getSource(), getDimCacheArgument(ctx)))
                        .then(literal(INFO).executes(ctx -> promptDimensionInfo(ctx.getSource(), getDimCacheArgument(ctx))))
                        /* /wp dimension <dim> activate */
                        .then(literal(ENABLE)
                                // TODO: Add toggle cmd
                                .executes(ctx -> setActiveState(ctx.getSource(), getDimCacheArgument(ctx), true))
                                .then(Commands.argument(ENABLE.toString(), BoolArgumentType.bool())
                                        .executes(ctx -> setActiveState(ctx.getSource(), getDimCacheArgument(ctx), getEnableArgument(ctx)))))
                        .then(literal(LIST)
                                .then(literal(REGION).executes(ctx -> promptDimensionRegionList(ctx.getSource(), getDimCacheArgument(ctx))))
                                /* /wp dimension <dim> list owner */
                                .then(literal(OWNER).executes(ctx -> promptDimensionPlayerList(ctx.getSource(), getDimCacheArgument(ctx), OWNER)))
                                /* /wp dimension <dim> list member */
                                .then(literal(MEMBER).executes(ctx -> promptDimensionPlayerList(ctx.getSource(), getDimCacheArgument(ctx), MEMBER)))
                                /* /wp dimension <dim> list flag */
                                .then(literal(FLAG).executes(ctx -> promptDimensionFlagList(ctx.getSource(), getDimCacheArgument(ctx)))))
                        .then(literal(DELETE)
                                .then(Commands.argument(REGION.toString(), StringArgumentType.word())
                                        .suggests((ctx, builder) -> RegionArgumentType.region().listSuggestions(ctx, builder))
                                        .executes(ctx -> attemptDeleteRegion(ctx.getSource(), getDimCacheArgument(ctx), getRegionArgument(ctx)))
                                        .then(Commands.literal("-y")
                                                .executes(ctx -> deleteRegion(ctx.getSource(), getDimCacheArgument(ctx), getRegionArgument(ctx))))))
                        .then(literal(REMOVE)
                                .then(literal(PLAYER)
                                        .then(Commands.argument(AFFILIATION.toString(), StringArgumentType.string())
                                                .suggests((ctx, builder) -> SharedSuggestionProvider.suggest(affiliationList, builder))
                                                .then(Commands.argument(PLAYER.toString(), EntityArgument.player())
                                                        .executes(ctx -> removePlayer(ctx.getSource(), getPlayerArgument(ctx), getDimCacheArgument(ctx), getAffiliationArgument(ctx)))))

                                        .then(Commands.argument(AFFILIATION.toString(), StringArgumentType.string())
                                                .suggests((ctx, builder) -> SharedSuggestionProvider.suggest(affiliationList, builder))
                                                .then(Commands.argument(PLAYER.toString(), EntityArgument.player())
                                                        .executes(ctx -> removePlayer(ctx.getSource(), getPlayerArgument(ctx), getDimCacheArgument(ctx), getAffiliationArgument(ctx))))))
                                .then(literal(TEAM)
                                        .then(Commands.argument(AFFILIATION.toString(), StringArgumentType.string())
                                                .suggests((ctx, builder) -> SharedSuggestionProvider.suggest(affiliationList, builder))
                                                .then(Commands.argument(TEAM.toString(), TeamArgument.team())
                                                        .executes(ctx -> removeTeam(ctx.getSource(), getTeamArgument(ctx), getDimCacheArgument(ctx), getAffiliationArgument(ctx)))))

                                        .then(Commands.argument(AFFILIATION.toString(), StringArgumentType.string())
                                                .suggests((ctx, builder) -> SharedSuggestionProvider.suggest(affiliationList, builder))
                                                .then(Commands.argument(TEAM.toString(), TeamArgument.team())
                                                        .executes(ctx -> removeTeam(ctx.getSource(), getTeamArgument(ctx), getDimCacheArgument(ctx), getAffiliationArgument(ctx))))))
                                .then(literal(FLAG)
                                        .then(Commands.argument(FLAG.toString(), StringArgumentType.string())
                                                .suggests((ctx, builder) -> SharedSuggestionProvider.suggest(RegionDataManager.get().getFlagsIdsForDim(getDimCacheArgument(ctx)), builder))
                                                .executes(ctx -> removeFlag(ctx.getSource(), getDimCacheArgument(ctx), StringArgumentType.getString(ctx, FLAG.toString()))))))
                        .then(literal(ADD)
                                .then(literal(PLAYER)
                                        .then(Commands.argument(AFFILIATION.toString(), StringArgumentType.string())
                                                .suggests((ctx, builder) -> SharedSuggestionProvider.suggest(affiliationList, builder))
                                                .then(Commands.argument(PLAYER.toString(), EntityArgument.player())
                                                        .executes(ctx -> addPlayer(ctx.getSource(), getPlayerArgument(ctx), getDimCacheArgument(ctx), getAffiliationArgument(ctx)))))

                                        .then(Commands.argument(AFFILIATION.toString(), StringArgumentType.string())
                                                .suggests((ctx, builder) -> SharedSuggestionProvider.suggest(affiliationList, builder))
                                                .then(Commands.argument(PLAYER.toString(), EntityArgument.player())
                                                        .executes(ctx -> addPlayer(ctx.getSource(), getPlayerArgument(ctx), getDimCacheArgument(ctx), getAffiliationArgument(ctx))))))
                                .then(literal(TEAM)
                                        .then(Commands.argument(AFFILIATION.toString(), StringArgumentType.string())
                                                .suggests((ctx, builder) -> SharedSuggestionProvider.suggest(affiliationList, builder))
                                                .then(Commands.argument(TEAM.toString(), TeamArgument.team())
                                                        .executes(ctx -> addTeam(ctx.getSource(), getTeamArgument(ctx), getDimCacheArgument(ctx), getAffiliationArgument(ctx)))))

                                        .then(Commands.argument(AFFILIATION.toString(), StringArgumentType.string())
                                                .suggests((ctx, builder) -> SharedSuggestionProvider.suggest(affiliationList, builder))
                                                .then(Commands.argument(TEAM.toString(), TeamArgument.team())
                                                        .executes(ctx -> addTeam(ctx.getSource(), getTeamArgument(ctx), getDimCacheArgument(ctx), getAffiliationArgument(ctx))))))
                                .then(literal(FLAG)
                                        .then(Commands.argument(FLAG.toString(), StringArgumentType.string())
                                                .suggests((ctx, builder) -> SharedSuggestionProvider.suggest(RegionFlag.getFlagNames(), builder))
                                                .executes(ctx -> addFlag(ctx.getSource(), getDimCacheArgument(ctx), StringArgumentType.getString(ctx, FLAG.toString())))))));
    }

    @Nullable
    public static int checkValidRegionName(String regionName, DimensionRegionCache dimCache) {
        if (!regionName.matches(RegionArgumentType.VALID_NAME_PATTERN.pattern())) {
            return -1;
        }
        if (dimCache.contains(regionName)) {
            return 1;
        }
        return 0;
    }

    private static int createCuboidRegion(CommandSourceStack src, String regionName, DimensionRegionCache dimCache, BlockPos pos1, BlockPos pos2, @Nullable ServerPlayer owner) {
        int res = checkValidRegionName(regionName, dimCache);
        if (res == -1) {
            sendCmdFeedback(src, new TranslatableComponent("cli.msg.dim.info.region.create.name.invalid", regionName));
            return res;
        }
        if (res == 1) {
            sendCmdFeedback(src, new TranslatableComponent("cli.msg.dim.info.region.create.name.exists", dimCache.getDimensionalRegion().getName(), regionName));
            return res;
        }
        CuboidRegion region = new CuboidRegion(regionName, new CuboidArea(pos1, pos2), owner, dimCache.dimensionKey());
        if (owner == null) {
            region.setIsActive(false);
        }
        RegionDataManager.addFlags(RegionConfig.getDefaultFlags(), region);
        dimCache.addRegion(region);
        LocalRegions.ensureHigherRegionPriorityFor(region, RegionConfig.DEFAULT_REGION_PRIORITY.get());
        RegionDataManager.save();
        sendCmdFeedback(src, new TranslatableComponent("cli.msg.dim.info.region.create.success", buildRegionInfoLink(region)));
        return 0;
    }

    private static int createSphereRegion(CommandSourceStack src, @Nonnull String regionName, DimensionRegionCache dimCache, BlockPos center, BlockPos outerPos, ServerPlayer owner) {
        if (!regionName.matches(RegionArgumentType.VALID_NAME_PATTERN.pattern())) {
            sendCmdFeedback(src, new TranslatableComponent("cli.msg.dim.info.region.create.name.invalid", regionName));
            return -1;
        }
        if (dimCache.contains(regionName)) {
            sendCmdFeedback(src, new TranslatableComponent("cli.msg.dim.info.region.create.name.exists", dimCache.dimensionKey(), regionName));
            return 1;
        }
        SphereArea area = new SphereArea(center, outerPos);
        SphereRegion region = new SphereRegion(regionName, area, owner, dimCache.dimensionKey());
        RegionDataManager.addFlags(RegionConfig.getDefaultFlags(), region);
        dimCache.addRegion(region);
        RegionDataManager.save();
        sendCmdFeedback(src, new TranslatableComponent("cli.msg.dim.info.region.create.success", buildRegionInfoLink(region)));
        return 0;
    }

    private static int attemptDeleteRegion(CommandSourceStack src, DimensionRegionCache dim, IMarkableRegion region) {
        if (dim.contains(region.getName())) {
            sendCmdFeedback(src, new TranslatableComponent("cli.msg.info.dim.region.remove.attempt", region.getName(), dim.dimensionKey().location()));
            return 0;
        }
        return 1;
    }

    // FIXME: Are child / parent relation properly removed when deleting a region?
    private static int deleteRegion(CommandSourceStack src, DimensionRegionCache dim, IMarkableRegion region) {
        if (dim.contains(region.getName())) {
            if (!region.getChildren().isEmpty()) {
                // TODO: config option which allows deleting region with children? children then default to dim parent
                sendCmdFeedback(src, new TranslatableComponent("cli.msg.info.dim.region.remove.fail.hasChildren", region.getName()));
                return -1;
            }
            if (region.getParent() != null) {
                region.getParent().removeChild(region);
                RegionDataManager.get().cacheFor(region.getDim()).getDimensionalRegion().addChild(region);
            }
            dim.removeRegion(region);
            sendCmdFeedback(src, new TranslatableComponent("cli.msg.info.dim.region.remove.confirm", region.getName(), dim.dimensionKey().location()));
            return 0;
        }
        return 1;
    }

    private static int removeFlag(CommandSourceStack src, DimensionRegionCache dimCache, String flag) {
        if (dimCache != null) {
            ResourceKey<Level> dim = dimCache.dimensionKey();
            dimCache.removeFlag(flag);
            sendCmdFeedback(src, new TranslatableComponent("cli.msg.flags.removed", flag, dim.location().toString()));
            return 0;
        }
        return 1;
    }

    private static int addFlag(CommandSourceStack src, DimensionRegionCache dimCache, String flag) {
        // FIXME: For now this works because we only have condition flags and no black/whitelist feature
        IFlag iflag = new BooleanFlag(flag, false);
        if (dimCache != null) {
            ResourceKey<Level> dim = dimCache.dimensionKey();
            dimCache.addFlag(iflag);
            sendCmdFeedback(src, new TranslatableComponent("cli.msg.flags.added", flag, dim.location().toString()));
            return 0;
        }
        return 1;
    }

    private static int removePlayer(CommandSourceStack src, ServerPlayer player, DimensionRegionCache dimCache, String affiliationType) {
        if (dimCache != null) {
            ResourceKey<Level> dim = dimCache.dimensionKey();
            if (affiliationType.equals(MEMBER.toString())) {
                dimCache.removeMember(player);
            }
            if (affiliationType.equals(OWNER.toString())) {
                dimCache.removeOwner(player);
            }
            sendCmdFeedback(src, new TranslatableComponent("cli.msg.dim.info.player.removed", affiliationType, player.getScoreboardName(), dim.location().toString()));
        }
        return 0;
    }

    private static int removeTeam(CommandSourceStack src, PlayerTeam team, DimensionRegionCache dimCache, String affiliationType) {
        if (dimCache != null) {
            ResourceKey<Level> dim = dimCache.dimensionKey();
            if (affiliationType.equals(MEMBER.toString())) {
                dimCache.removeMember(team);
            }
            if (affiliationType.equals(OWNER.toString())) {
                dimCache.removeOwner(team);
            }
            sendCmdFeedback(src, new TranslatableComponent("cli.msg.dim.info.player.removed", affiliationType, team.getName(), dim.location().toString()));
            return 0;
        }
        return 1;
    }

    // TODO: If works replace with switch and catch error
    private static int addPlayer(CommandSourceStack src, ServerPlayer player, DimensionRegionCache dimCache, String affiliationType) {
        if (dimCache != null) {
            ResourceKey<Level> dim = dimCache.dimensionKey();
            if (affiliationType.equals(MEMBER.toString())) {
                dimCache.addMember(player);
            }
            if (affiliationType.equals(OWNER.toString())) {
                dimCache.addOwner(player);
            }
            sendCmdFeedback(src, new TranslatableComponent("cli.msg.dim.info.player.added", player.getScoreboardName(), dim.location().toString(), affiliationType));
            return 0;
        }
        return 1;
    }

    private static int addTeam(CommandSourceStack src, PlayerTeam team, DimensionRegionCache dimCache, String affiliationType) {
        if (dimCache != null) {
            ResourceKey<Level> dim = dimCache.dimensionKey();
            if (affiliationType.equals(MEMBER.toString())) {
                dimCache.addMember(team);
            }
            if (affiliationType.equals(OWNER.toString())) {
                dimCache.addOwner(team);
            }
            sendCmdFeedback(src, new TranslatableComponent("cli.msg.dim.info.team.added", team.getName(), dim.location().toString(), affiliationType));
            return 0;
        }
        return 1;
    }

    private static int promptDimensionFlagList(CommandSourceStack src, DimensionRegionCache dimCache) {
        List<IFlag> activeFlags = dimCache.getDimensionalRegion().getFlags().stream()
                .filter(IFlag::isActive)
                .sorted()
                .collect(Collectors.toList());
        List<IFlag> inActiveFlags = dimCache.getDimensionalRegion().getFlags().stream()
                .filter(f -> !f.isActive())
                .sorted()
                .collect(Collectors.toList());
        activeFlags.addAll(inActiveFlags);
        List<IFlag> flags = new ArrayList<>(activeFlags);
        flags.addAll(inActiveFlags);

        ResourceKey<Level> dim = dimCache.dimensionKey();
        if (flags.isEmpty()) {
            sendCmdFeedback(src, new TranslatableComponent("cli.msg.dim.info.flags.empty", dim.location().toString()));
            return 1;
        }
        MutableComponent dimInfoLink = buildDimensionalInfoLink(dim);
        MutableComponent headerContent = new TranslatableComponent("cli.msg.info.region.flag.header", dimInfoLink);
        sendCmdFeedback(src, headerContent);
        flags.forEach(flag -> {
            MutableComponent removeFlagLink = new TextComponent(" - ")
                    .append(buildDimensionRemoveFlagLink(flag, dim))
                    .append(new TextComponent(" '" + flag.getFlagIdentifier() + "' "));

            sendCmdFeedback(src, removeFlagLink);
        });
        return 0;
    }

    private static int promptDimensionPlayerList(CommandSourceStack src, DimensionRegionCache dimCache, CommandConstants memberOrOwner) {
        if (dimCache != null) {
            DimensionalRegion dimRegion = dimCache.getDimensionalRegion();
            String playerLangKeyPart = memberOrOwner == OWNER ? "owner" : "member";
            String affiliationText = playerLangKeyPart.substring(0, 1).toUpperCase() + playerLangKeyPart.substring(1) + "s";
            MutableComponent dimInfoLink = buildDimensionalInfoLink(dimRegion.getDim());
            MutableComponent regionsInDimHeader = new TranslatableComponent("cli.msg.info.region.affiliation.player.list", affiliationText, dimInfoLink);
            sendCmdFeedback(src, regionsInDimHeader);
            sendCmdFeedback(src, buildTeamList(dimRegion, memberOrOwner));
            sendCmdFeedback(src, buildPlayerList(dimRegion, memberOrOwner));
            return 0;
        }
        return 1;
    }

    private static int setActiveState(CommandSourceStack src, DimensionRegionCache dimCache, boolean activate) {
        if (dimCache != null) {
            dimCache.setDimState(activate);

            String langKey = "cli.msg.info.state." + (activate ? "activated" : "deactivated");
            sendCmdFeedback(src, new TranslatableComponent(langKey, dimCache.getDimensionalRegion().getDim().location().toString()));
            return 0;
        }
        return 1;
    }

    private static int promptDimensionRegionList(CommandSourceStack source, DimensionRegionCache dimCache) {
        if (dimCache != null) {
            ResourceKey<Level> dim = dimCache.getDimensionalRegion().getDim();
            List<IMarkableRegion> regionsForDim = dimCache.regionsInDimension
                    .values()
                    .stream()
                    .sorted(Comparator.comparing(IMarkableRegion::getName))
                    .collect(Collectors.toList());
            if (regionsForDim.isEmpty()) {
                sendCmdFeedback(source, new TranslatableComponent("cli.msg.dim.info.regions.empty", dim.location().toString()));
                return -1;
            }
            MutableComponent dimInfoLink = buildDimensionalInfoLink(dim);
            MutableComponent regionsInDimHeader = new TranslatableComponent("cli.msg.dim.info.region.list.header", dimInfoLink);
            sendCmdFeedback(source, regionsInDimHeader);
            // TODO: Pagination for more than x regions
            regionsForDim.forEach(region -> {
                MutableComponent regionRemoveLink = new TextComponent(" - ")
                        .append(buildDimSuggestRegionRemovalLink(region))
                        .append(" ")
                        .append(buildRegionInfoLink(region))
                        .append(dimCache.getDimensionalRegion().hasChild(region)
                                ? MessageUtil.buildTextWithHoverMsg(new TextComponent("*"), new TranslatableComponent("cli.msg.info.dim.region.child.hover"), GOLD)
                                : new TextComponent(""))
                        .append(new TextComponent(RESET + " @ " + RESET))
                        .append(buildRegionTeleportLink(region));
                sendCmdFeedback(source, regionRemoveLink);
            });
            return 0;
        }
        return 1;
    }

    /* Used for dimension info */
    private static void promptDimensionOwners(CommandSourceStack src, DimensionalRegion dimRegion) {
        // [n player(s)] [+]
        PlayerContainer owners = dimRegion.getOwners();
        MutableComponent playersAddLink = buildDimAddPlayerLink(dimRegion, "cli.msg.dim.info.players.add",
                OWNER);
        MutableComponent players = owners.hasPlayers()
                ? buildDimPlayerListLink(dimRegion, owners, OWNER)
                : new TranslatableComponent("cli.msg.info.region.affiliation.player.list.link.text", owners.getPlayers().size());
        players.append(playersAddLink);

        // [n team(s)] [+]
        MutableComponent teamAddLink = buildDimAddTeamLink(dimRegion, "cli.msg.dim.info.teams.add",
                OWNER);
        MutableComponent teams = owners.hasTeams()
                ? buildDimTeamListLink(dimRegion, owners, OWNER)
                : new TranslatableComponent("cli.msg.info.region.affiliation.team.list.link.text", owners.getTeams().size());
        teams.append(teamAddLink);

        // Owners: [n player(s)] [+], [n team(s)] [+]
        MutableComponent dimOwners = new TranslatableComponent("cli.msg.dim.info.owners")
                .append(new TextComponent(": "))
                .append(players).append(new TextComponent(", "))
                .append(teams);
        sendCmdFeedback(src, dimOwners);
    }

    private static int promptDimensionRegions(CommandSourceStack source, DimensionRegionCache dimCache) {
        if (dimCache != null) {
            ResourceKey<Level> dim = dimCache.getDimensionalRegion().getDim();
            List<IMarkableRegion> regionsForDim = dimCache.regionsInDimension
                    .values()
                    .stream()
                    .sorted(Comparator.comparing(IMarkableRegion::getName))
                    .collect(Collectors.toList());
            MutableComponent regions = new TranslatableComponent("cli.msg.info.dim.region").append(": ");
            if (regionsForDim.isEmpty()) {
                regions.append(new TranslatableComponent("cli.msg.dim.info.regions.empty", dim.location().toString()));
            } else {
                regions.append(buildDimRegionListLink(dimCache, dimCache.getDimensionalRegion()));
            }
            sendCmdFeedback(source, regions);
            return 0;
        }
        return 1;
    }

    private static void promptDimensionMembers(CommandSourceStack src, DimensionalRegion dimRegion) {
        // [n player(s)] [+]
        PlayerContainer members = dimRegion.getMembers();
        MutableComponent playersAddLink = buildDimAddPlayerLink(dimRegion, "cli.msg.dim.info.players.add",
                MEMBER);
        MutableComponent players = members.hasPlayers() ?
                buildDimPlayerListLink(dimRegion, members, MEMBER)
                : new TranslatableComponent("cli.msg.info.region.affiliation.player.list.link.text", members.getPlayers().size());
        players.append(playersAddLink);

        // [n team(s)] [+]
        MutableComponent teamAddLink = buildDimAddTeamLink(dimRegion, "cli.msg.dim.info.teams.add",
                MEMBER);
        MutableComponent teams = members.hasTeams()
                ? buildDimTeamListLink(dimRegion, members, MEMBER)
                : new TranslatableComponent("cli.msg.info.region.affiliation.team.list.link.text", members.getTeams().size());
        teams.append(teamAddLink);

        // Members: [n player(s)] [+], [n team(s)] [+]
        MutableComponent dimMembers = new TranslatableComponent("cli.msg.dim.info.members")
                .append(new TextComponent(": "))
                .append(players).append(new TextComponent(", "))
                .append(teams);
        sendCmdFeedback(src, dimMembers);
    }

    private static void promptDimensionFlags(CommandSourceStack src, DimensionalRegion dimRegion) {
        MutableComponent dimFlagMessage = new TranslatableComponent("cli.msg.dim.info.flags");
        MutableComponent flags = dimRegion.getFlags().isEmpty()
                ? new TranslatableComponent("cli.msg.info.region.flag.link.text", dimRegion.getFlags().size())
                : buildDimFlagListLink(dimRegion);
        dimFlagMessage.append(new TextComponent(": "))
                .append(flags)
                .append(buildAddDimFlagLink(dimRegion));
        sendCmdFeedback(src, dimFlagMessage);
    }

    private static void promptDimensionState(CommandSourceStack src, AbstractRegion region, String command) {
        String onClickAction = region.isActive() ? "deactivate" : "activate";
        String hoverText = "cli.msg.info.state." + onClickAction;
        String linkText = "cli.msg.info.state.link." + (region.isActive() ? "activate" : "deactivate");
        ChatFormatting color = region.isActive() ? GREEN : RED;
        MutableComponent stateLink = buildExecuteCmdComponent(linkText, hoverText, command, ClickEvent.Action.RUN_COMMAND, color);
        sendCmdFeedback(src, new TranslatableComponent("cli.msg.info.state")
                .append(new TextComponent(": "))
                .append(stateLink));
    }

    private static int promptDimensionInfo(CommandSourceStack src, DimensionRegionCache dimCache) {
        // Dimension info header
        DimensionalRegion dimRegion = dimCache.getDimensionalRegion();
        MutableComponent clipBoardDumpLink = buildExecuteCmdComponent("cli.msg.dim.overview.header.dump.link.text", "cli.msg.dim.overview.header.dump.link.hover", NbtUtils.prettyPrint(dimRegion.serializeNBT()), ClickEvent.Action.COPY_TO_CLIPBOARD, GOLD);
        MutableComponent dimInfoHeader = new TranslatableComponent("cli.msg.dim.overview.header", clipBoardDumpLink, buildDimensionalInfoLink(dimRegion.getDim()));
        sendCmdFeedback(src, dimInfoHeader);

        // Regions in dimension
        // TODO: Change [n region(s)] to [n region(s)] [+]s
        promptDimensionRegions(src, dimCache);

        // Dimension owners & members
        promptDimensionOwners(src, dimRegion);
        promptDimensionMembers(src, dimRegion);

        // Flags: [n flag(s)] [+]
        promptDimensionFlags(src, dimRegion);

        // State: [activated]
        String command = "/" + CommandPermissionConfig.WP + " " + DIMENSION + " " + dimRegion.getName() + " " + ENABLE + " " + !dimRegion.isActive();
        promptDimensionState(src, dimRegion, command);
        return 0;
    }
}
